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

# The Makefile for the basic examples and its subdirectories.

# $Id$

# To view the demos, use ../advi.opt file.dvi

SUBDIRS=cash

TEXFILES= simple.tex moderate.tex bubbles.tex

ADVIDIR=../..

include ../../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): latex
PLATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): platex

DVIFILES= $(TEXFILES:.tex=.dvi)
ADVI=TEXPSHEADERS=$(ADVIDIR)/tex: $(ADVIDIR)/advi

MLPROG=

all: $(DVIFILES)
	advi simple.dvi && \
	advi moderate.dvi && \
	advi bubbles.dvi 

dvi : $(DVIFILES) $(MLPROG)

$(DVIFILES): $(ADVIDIR)/tex/bar.eps $(ADVIDIR)/tex/advi.sty $(ADVIDIR)/tex/advi-annot.sty 

.dvi.advi:
	$(ADVI) $<

.tex.dvi:
	$(LATEX) $<

clean:
	$(RM) a.out *.aux *.log *.dvi *~ 
	$(RM) $(MLPROG) *.cmi *.cmo
	$(RM) .advi/*
	for i in $(SUBDIRS); do \
	  cd $$i; $(MAKE) clean; cd ..; \
	done

.SUFFIXES: .tex .sty .dvi .eps .advi
