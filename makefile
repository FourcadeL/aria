all: usage

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

parser.ml: parser.mly
	ocamlyacc -v $<

lexer.ml: lexer.mll
	ocamllex $<

audio.cmo: audio.cmi audio.ml

parser.cmo: audio.cmi parser.cmi

parser.cmi: parser.mli audio.cmo
	ocamlc -c $<

test_parser.cmo : parser.cmi lexer.cmo

test_parser: audio.cmo parser.cmo lexer.cmo test_parser.cmo
	ocamlc -o $@ $^


clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi test_parser parser.output



usage:
	@echo "make for tool compiling"