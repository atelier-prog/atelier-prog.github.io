let ( >>= ) = Lwt.bind

module String =
struct

  include String
  let caml = Js.to_string
  let js = Js.string

  let txt v =
    Dom_html.document ## createTextNode (js v)

end

let _s = String.js
let s_ = String.caml


let doc = Dom_html.document
let window = Dom_html.window
let alert m = window ## alert (String.js m)
let log x = Firebug.console ## log(x)

module Html =
struct

  exception Element_Not_found
  let fail () = raise Element_Not_found
  let unopt x = Js.Opt.get x fail

  let get_by_id id =
    try
      let r = Dom_html.document ## getElementById (String.js id) in
      Some (unopt r)
    with _ -> None

  let find container selector =
    try
      let r = container ## querySelector (String.js selector) in
      Some (unopt r)
    with _ -> None

  let select container selector =
    container ## querySelectorAll (String.js selector)
    |> Dom.list_of_nodeList


  let get_attribute elt attr =
    let s_attr = _s attr in
    if (elt ## hasAttribute (s_attr)) == Js._true
    then
      Some (
        elt ## getAttribute (s_attr)
        |> unopt
        |> s_
      )
    else None


  let set_attribute elt attr value =
    let s_attr = _s attr
    and s_value = _s value in
    elt ## setAttribute(s_attr, s_value)

  let get_data elt data =
    let attr = "data-"^data in
    get_attribute elt attr

  let set_data elt data value =
    let attr = "data-"^data in
    set_attribute elt attr value

  let to_img x =
    Dom_html.CoerceTo.img x
    |> unopt

  let iter_children f node =
    let nodeL = node ## childNodes in
    let len = nodeL ## length in
    for i = 0 to (pred len) do
      Js.Opt.iter (nodeL ## item(i)) f
    done

  let remove_children fnode =
    let rec iter node =
      match Js.Opt.to_option (node ## firstChild) with
      | None -> ()
      | Some child ->
        let _ = node ## removeChild(child) in iter node
    in iter fnode

end

module Ajax =
struct

  let load file =
  let open XmlHttpRequest in
  get file >>= (fun frame ->
      let code = frame.code
      and message = frame.content in
      if code = 0 || code = 200
      then Lwt.return (Some message)
      else Lwt.return None
    )

end

module Gravatar =
struct

  let md5 str =
    let value = String.(trim (lowercase str)) in
    Digest.(to_hex (string value))

  let uri_for ?(dim=100) email =
    let md5_email = md5 email in
    (Printf.sprintf
      "http://www.gravatar.com/avatar/%s?s=%d&d=%s"
      md5_email
      dim
      "identicon")
    |> String.js

  (* let substitue () = *)
  (*   let img = Html.select doc "img" in *)
  (*   List.iter *)
  (*     (fun i -> *)
  (*        let ig = Html.to_img i in *)
  (*        match (Html.get_data ig "avatar") with *)
  (*        | None -> () *)
  (*        | Some x -> ig ## src <- (uri_for x) *)
  (*     img *)

end


module Post =
struct
  type t = {
    url : string
  ; title : string
  ; author : string
  ; email_base : string
  ; email_domain : string
  ; reference : string list
  } deriving (Yojson)
  type posts = t list deriving (Yojson)
  let ptl = Yojson.from_string<posts>

end

include Post

let bind_event base post li =
  let open Lwt_js_events in
  async_loop click li (fun a b ->
      let _ = Html.remove_children base in
      let _ = match Html.get_by_id "subtitle" with
        | None -> ()
        | Some subtitle ->
          let title = post.title ^ ", par " ^ post.author in
          let _ = Html.remove_children subtitle in
          let _ = Dom.appendChild subtitle (String.txt title) in
          let mdiv = Dom_html.createDiv doc in
          let _ =
            Ajax.load post.url
            >>= ( function
                | None -> alert "Failed to load document"; Lwt.return_unit
                | Some content -> log content; Lwt.return_unit
              ) in Dom.appendChild base mdiv
      in
      Lwt.return_unit
    )


let a_post base ul post =
  let email = post.email_base ^ "@" ^ post.email_domain in
  let img = Dom_html.createImg doc in
  let _ = img ## src <- (Gravatar.uri_for email) in
  let li = Dom_html.createLi doc in
  let h3 = Dom_html.createH3 doc in
  let _ = Dom.appendChild h3 (String.txt post.title) in
  let mdiv = Dom_html.createDiv doc in
  let _ = Dom.appendChild mdiv h3 in
  let _ = Dom.appendChild mdiv (String.txt ("Posté par "^post.author)) in
  let _ = Dom.appendChild li img in
  let _ = Dom.appendChild li mdiv in
  let _ = bind_event base post li in
  Dom.appendChild ul li

let perform_blog_post blogposts =
  match (Html.get_by_id "content-blog", Html.get_by_id "full-content") with
  | None, _ | _, None -> ()
  | Some ul, Some base ->
    let list = Post.ptl blogposts in
    let _ = List.iter (a_post base ul) list in
    ()


(* Initialization callback *)
let initialize () =
  let _ =
    Ajax.load "posts.json"
    >>= (fun x ->
        let _ = match x with
          | Some ms ->
            perform_blog_post ms
          | None -> alert "Failed to load blog post list"
        in
        Lwt.return_unit
      )

  in
  Lwt.return ()

(* General promise *)
let () =
  let dom_ready () =
    let time, u = Lwt.wait () in
    let _ = window ## onload <-
        Dom.handler (fun _ -> Lwt.wakeup u (); Js._true)
    in time
  in
  (dom_ready ()) >>= initialize
  |> ignore
