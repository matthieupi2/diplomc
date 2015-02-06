JEUCMO = interp.cmo parser.cmo lexer.cmo jeu.cmo
MINICCMO = interp.cmo parser.cmo lexer.cmo minic.cmo
GENERATED = lexer.ml parser.ml parser.mli

all: jeu minic

jeu: $(JEUCMO)
	ocamlc -o $@ $(JEUCMO)

minic: $(MINICCMO)
	ocamlc -o $@ $(MINICCMO)

.SUFFIXES: .mli .ml .cmi .cmo

.mli.cmi:
	ocamlc -c $<

.ml.cmo:
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly ast.cmi
	menhir --infer parser.mly
parser.mli: parser.mly ast.cmi
	menhir --infer parser.mly

clean:
	rm -f *.cm[io] *~ .depend jeu minic $(GENERATED)

.depend: $(GENERATED)
	ocamldep *.ml *.mli > .depend

include .depend
