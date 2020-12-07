#!/usr/bin/make -f

# $Id: Makefile,v 1.5 2005/10/30 21:47:09 jnarboux Exp $ 
# Building Automatic Theorem Prover (ATP)

SOURCES=

SOURCES+=lib.ml 
SOURCES+=intro.ml 
SOURCES+=formulas.ml 
SOURCES+=fol.ml 
SOURCES+=order.ml 
SOURCES+=cooper.ml  
SOURCES+=prop.ml
SOURCES+=grobner.ml  
SOURCES+=geom.ml   

LIBS=nums 
OCAMLBLDFLAGS =
OCAMLNLDFLAGS =

RESULT=libATP

all: byte opt

byte: bcl

opt: ncl

graphdot:
	ocamldep *.mli *.ml > atp-graph.dep
	ocamldot atp-graph.dep > atp-graph.dot
	# to display the dep graph, use :
	# dotty atp.dot 

latotale: clean all graphdot


include ./OCamlMakefile

include ./Makefile.odoc
