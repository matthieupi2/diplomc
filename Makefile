CMO = interp.cmo parser.cmo lexer.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli

all: jeu

jeu: $(CMO)
	ocamlc -o $@ $(CMO)

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
	rm -f *.cm[io] *~ .depend jeu $(GENERATED)

.depend: $(GENERATED)
	ocamldep *.ml *.mli > .depend

include .depend
