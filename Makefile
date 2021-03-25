SOURCES = tetris.ml
PACKS = universeJs
RESULT = game
OCAMLMAKEFILE = /Users/shibatamakoto/.opam/4.04.0/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)

$(RESULT).js : byte-code
	js_of_ocaml $(RESULT)
