FIND=ocamlfind ocamlc
JSDEP=-package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg
JSOO=$(FIND) $(JSDEP)

.PHONY: all
all:
	$(JSOO) -o blog.byte src-blog/blog.ml
	js_of_ocaml blog.byte -o blog/blog.js
