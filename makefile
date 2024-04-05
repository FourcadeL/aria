SRCDIR := sources

PARSERSOURCES = $(shell find $(SRCDIR) -name "*.mly")
LEXERSOURCES = $(shell find $(SRCDIR) -name "*.mll")
CAMLSOURCES = $(shell find $(SRCDIR) -name "*.ml")
CAMLINTERFACES = $(shell find $(SRCDIR) -name "*.mli")



all: usage

$(SRCDIR)/%.cmi: $(SRCDIR)/%.mli
	ocamlc -c $<

$(SRCDIR)/%.cmo: $(SRCDIR)/%.ml
	ocamlc -c $<

$(SRCDIR)/parser.ml: $(SRCDIR)/parser.mly
	ocamlyacc -v $<

$(SRCDIR)/lexer.ml: $(SRCDIR)/lexer.mll
	ocamllex $<

$(SRCDIR)/audio.cmo: $(SRCDIR)/audio.cmi $(SRCDIR)/audio.ml

$(SRCDIR)/parser.cmo: $(SRCDIR)/audio.cmi $(SRCDIR)/parser.cmi

$(SRCDIR)/parser.cmi: $(SRCDIR)/parser.mli $(SRCDIR)/audio.cmo
	ocamlc -c $<

$(SRCDIR)/test_parser.cmo : $(SRCDIR)/parser.cmi $(SRCDIR)/lexer.cmo

test_parser: $(SRCDIR)/audio.cmo $(SRCDIR)/parser.cmo $(SRCDIR)/lexer.cmo $(SRCDIR)/test_parser.cmo
	@ocamlc -o $@ $^
	@clean


clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi test_parser parser.output



usage:
	@echo "make \"test_parser\" for tool compiling"