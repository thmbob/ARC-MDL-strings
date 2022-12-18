
OBJ=bintree.cmo common.cmo xprint.cmo iterator.cmo myseq.cmo mymap.cmo mdl.cmo find_merge.cmo intmap.cmo
SRC=expr.cmo task.cmo range.cmo types_utilities.cmo printing.cmo parsing.cmo environment.cmo dl_compute.cmo writing.cmo reading.cmo refinement.cmo model.cmo pretty_printing.ml
SRC_FABLIS=focus.cmo syntax.cmo lis.cmo jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo

#test :
SRC_TEST_PARSING=expr.cmo types_utilities.cmo printing.cmo parsing.cmo
SRC_TEST_APPLY_REFINEMENTS=expr.cmo types_utilities.cmo printing.cmo parsing.cmo environment.cmo dl_compute.cmo writing.cmo
SRC_TEST_REFINING=expr.cmo task.cmo types_utilities.cmo printing.cmo parsing.cmo environment.cmo dl_compute.cmo writing.cmo reading.cmo  refinement.cmo

FLAGS= -I ../../../../../.seb-ferre-caml-lib/ocaml-lib -I ../../../../../../usr/lib/ocaml -package unix,str,stdio,num,yojson,ppx_deriving_yojson
FLAGS_FABLIS = -I ../../../../../.seb-ferre-caml-lib/fablis/core -I ../../../../../.seb-ferre-caml-lib/fablis/core/webapp -I ../../../../../../usr/lib/ocaml
#JSOO_FLAGS = -pretty -no-inline -debug-info # for dev
JSOO_FLAGS = --opt 3 # for prod

lis: $(SRC) string_lis.ml
# requires to set profiling off (Common.prof_on := false)
	ocamlfind ocamlc $(FLAGS) $(FLAGS_FABLIS) -package js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -linkpkg -o html/script.byte $(OBJ) $(SRC) $(SRC_FABLIS) string_lis.ml
	js_of_ocaml $(JSOO_FLAGS) html/script.byte

test_parsing: $(SRC_TEST_PARSING:.cmo=.cmx) tests/test_parsing.ml
	ocamlfind ocamlopt $(FLAGS) -linkpkg -o tests/test_parsing $(OBJ:.cmo=.cmx) $(SRC_TEST_PARSING:.cmo=.cmx) tests/test_parsing.ml

test_apply_refinements: $(SRC_TEST_APPLY_REFINEMENTS:.cmo=.cmx) tests/test_apply_refinements.ml
	ocamlfind ocamlopt $(FLAGS) -linkpkg -o tests/test_apply_refinements $(OBJ:.cmo=.cmx) $(SRC_TEST_APPLY_REFINEMENTS:.cmo=.cmx) tests/test_apply_refinements.ml

test_refining: $(SRC_TEST_REFINING:.cmo=.cmx) tests/test_refining.ml
	ocamlfind ocamlopt $(FLAGS) -linkpkg -o tests/test_refining $(OBJ:.cmo=.cmx) $(SRC_TEST_REFINING:.cmo=.cmx) tests/test_refining.ml

clean:
	rm -f *.cm[ioax]
	rm -f tests/*.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<

%.cmx: %.ml
	ocamlfind ocamlopt -c $(FLAGS) $<
