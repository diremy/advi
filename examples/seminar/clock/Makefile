#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2003 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
# Pierre Weis                                                         #
#                                                                     #
#*********************************************************************#

# The Makefile for the clock example.

# $Id$

# To view the demos, use ../../../advi.opt file.dvi

TOPDIR=../../..
FILES=tools.tex
EPS=bar.jpg.eps cameleon.eps cameleon2.eps hump.eps logo.gif.eps ocaml.eps ocamldoc.eps ODBC.eps

include ../../lib/Makefile

all:: tools.dvi
	advi tools.dvi

tools.dvi: $(EPS) tools.tex
	$(LATEX) tools.tex

tools.pdf: $(EPS) tools.tex
	$(LATEX) '\def\argv{ps2pdf}\input' tools.tex
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi 

$(EPS):$(EPS.eps=.eps.gz)
	gunzip *.eps.gz

clean::
	for i in $(EPS); do \
	  if test -f $$i; \
	then \
	 gzip $$i; \
	else \
	 echo $$i; \
	fi; \
	done
	rm -f *.dvi *.pdf *.aux *.log *.ps *~

.phony: show
