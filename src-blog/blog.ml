let ( >>= ) = Lwt.bind

module String =
struct

  include String
  let caml = Js.to_string
  let js = Js.string

end

let _s = String.js
let s_ = String.caml


let doc = Dom_html.document
let window = Dom_html.window
let alert m = window ## alert (String.js m)

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

end

module Gravatar =
struct

  let md5 str =
    let value = String.(trim (lowercase str)) in
    Digest.(to_hex (string value))

  let uri_for ?(dim=100) email =
    let md5_email = md5 email in
    Printf.sprintf
      "http://www.gravatar.com/avatar/%s?s=%d&d=%s"
      md5_email
      dim
      "identicon"

  let substitue () =
    let img = Html.select doc "img" in
    List.iter
      (fun i ->
         let ig = Html.to_img i in
         match (Html.get_data ig "avatar") with
         | None -> ()
         | Some x -> ig ## src <- (_s (uri_for x))
      )
      img

end


(* Initialization callback *)
let initialize () =
  let _ = Gravatar.substitue () in
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
