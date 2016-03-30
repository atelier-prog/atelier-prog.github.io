FIND=ocamlfind ocamlc
JSDEP=-package js_of_ocaml -package js_of_ocaml.syntax
JSDEP+= -package deriving
JSDEP+= -package deriving-yojson
JSDEP+= -package deriving-yojson.syntax
JSDEP+= -syntax camlp4o -linkpkg
JSOO=$(FIND) $(JSDEP)

.PHONY: all blog


all: blog
	python -m SimpleHTTPServer 7777

blog:
	$(JSOO) -o blog.byte src-blog/blog.ml
	js_of_ocaml blog.byte -o blog/blog.js

