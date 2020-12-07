#!/usr/bin/make -f

# $Id: Makefile,v 1.3 2005/10/29 08:48:22 gmariano Exp $

#include OCamlMakefile

TOOLS=../tools

SVGCONVERT=@$(TOOLS)/svg_converter


ICONS=$(wildcard *.svg)


INCDIRS = +lablrsvg  +lablgtk2 #+gmp # +gettext +gettext-stub 

LIBS = lablrsvg  #gmp #gettextBase gettext-stub

SOURCES :=  ../debug.ml icons.ml $(ICONS:.svg=.ml)

RESULT=icons

all: convert $(RESULT)

convert: $(SOURCES)

include ./OCamlMakefile

.SUFFIXES: .ml .svg .cmx

.svg.ml:
	$(SVGCONVERT) $<


