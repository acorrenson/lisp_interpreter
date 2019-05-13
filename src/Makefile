
calc: ast.cmo parser.cmo lexer.cmo
	ocamlc -o calc ast.cmo parser.cmo lexer.cmo calc.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

parser:
	ocamlyacc parser.mly

parser.cmi: parser
	ocamlc -c parser.mli

parser.cmo: parser.cmi 
	ocamlc -c parser.ml

ast.cmo: ast.ml
	ocamlc -c ast.ml


clean:
	rm -rf *.cmi *.cmo *.mli lexer.ml parser.ml calc