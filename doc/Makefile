#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

LATEX=TEXINPUTS=.:../tex: latex
DVIPS=TEXPSHEADERS=.:../tex: dvips
HEVEA=hevea
IMAGEN=imagen
PDFIMAGEN=pdfimagen
DVIPDFM=dvipdfm

all: html manual.ps manual.pdf splash.dvi splash.ps mathmode.dvi

clean: 
	rm -f *.aux *.log manual.html *.dvi *.pdf *.pdfm *.ps *.ima *.out *.toc
	$(MAKE) -f MakeHTML clean

html:
	$(MAKE) -f MakeHTML all

splash.dvi manual.dvi: splash_contents.tex

manual.dvi: manual.sty

.SUFFIXES: .tex .dvi .advi .ps .pdf .pdfm .html .ima

.tex.dvi:
	echo $<
	$(LATEX) $<

.tex.ps:
	$(LATEX) '\def\driver{dvips}\input' $<
	$(DVIPS) -o $@ $*.dvi

.tex.pdf:
	$(LATEX) '\def\driver{dvipdfm}\input' $<
	- $(DVIPDFM) -o $@ $*.dvi

.advi.dvi:
	cp -p $< $*.dvi

.html.ima:
	if [ -f $*.image.tex ]; then $(PDFIMAGEN) $*; fi
	touch $*.ima

.html.tex:
	$(HEVEA) -o $*.html $*.tex
	if [ -f $<.image.tex ]; then mv $<.image.tex $*.image.tex; fi
	if [ -f $*.image.tex ]; then $(IMAGEN) $*; fi

