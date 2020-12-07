SOURCES =   ../lib/cdk/string2.mli ../lib/cdk/string2.ml \
	    ../lib/cdk/filename2.mli ../lib/cdk/filename2.ml \
	    svg_converter.ml
RESULT = svg_converter
PREFIX=$(DESTDIR)/usr
BINDIR=$(PREFIX)/bin
all: native-code
-include OCamlMakefile

install:
	cp $(RESULT) $(BINDIR)/
