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

audio.cmo: audio.cmi audio.ml

checker.cmo: checker.cmi checker.ml audio.cmi binaryTranslator.cmi

binaryTranslator.cmo: binaryTranslator.cmi binaryTranslator.ml audio.cmi

compress.cmo: compress.cmi compress.ml audio.cmi

rgbdsFileGenerator.cmo: rgbdsFileGenerator.cmi rgbdsFileGenerator.ml binaryTranslator.cmi audio.cmi

parser.cmo: audio.cmi parser.cmi

parser.cmi: parser.mli audio.cmo
	ocamlc -c $<

test_parser.cmo : parser.cmi lexer.cmo test_parser.ml

test_parser: audio.cmo parser.cmo lexer.cmo test_parser.cmo
	ocamlc -o $@ $^

main: audio.cmo parser.cmo lexer.cmo binaryTranslator.cmo checker.cmo compress.cmo rgbdsFileGenerator.cmo main.cmo
	ocamlc -o $@ $^

clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi test_parser parser.output main



usage:
	@echo "make \"test_parser\" for tool compiling or make \"main\""