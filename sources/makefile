SRCDIR := sources

PARSERSOURCES = $(shell find $(SRCDIR) -name "*.mly")
LEXERSOURCES = $(shell find $(SRCDIR) -name "*.mll")
CAMLSOURCES = $(shell find $(SRCDIR) -name "*.ml")
CAMLINTERFACES = $(shell find $(SRCDIR) -name "*.mli")



all: usage

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

parser.ml: parser.mly
	ocamlyacc -v $<

lexer.ml: lexer.mll
	ocamllex $<

audio.cmo: audio.cmi audio.ml ast.cmi

ast.cmo: ast.cmi ast.ml audio.cmi

analyser.cmo: analyser.cmi analyser.ml ast.cmi

checker.cmo: checker.cmi checker.ml audio.cmi binaryTranslator.cmi

binaryTranslator.cmo: binaryTranslator.cmi binaryTranslator.ml audio.cmi

compiler.cmo: compiler.cmi compiler.ml audio.cmi ast.cmi

compress.cmo: compress.cmi compress.ml audio.cmi

transform.cmo: transform.cmi transform.ml ast.cmi

rgbdsFileGenerator.cmo: rgbdsFileGenerator.cmi rgbdsFileGenerator.ml binaryTranslator.cmi audio.cmi

parser.cmo: audio.cmi ast.cmi parser.cmi

parser.cmi: parser.mli audio.cmo ast.cmo
	ocamlc -c $<

test_parser.cmo : parser.cmi lexer.cmo test_parser.ml

test_parser: audio.cmo ast.cmo parser.cmo lexer.cmo test_parser.cmo
	ocamlc -o $@ $^
	@make clean

aria: audio.cmo parser.cmo lexer.cmo binaryTranslator.cmo checker.cmo analyser.cmo compress.cmo transform.cmo compiler.cmo ast.cmo rgbdsFileGenerator.cmo main.cmo
	ocamlc -o $@ $^
	# @make clean

clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi parser.output



usage: aria
	@echo "make \"test_parser\" for tool compiling test"