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

include ../Makefile.config

# See also file MakeHTML to recompile HTML files for the Web site.

SETTEXINPUTS=TEXINPUTS=.:../tex:/usr/lib/hevea/:/usr/local/lib/hevea/:/usr/local/share/texmf/tex//:$$TEXINPUTS

LATEX=$(SETTEXINPUTS) latex
DVIPS=TEXPSHEADERS=.:../tex: dvips
HEVEA=$(PATH_HEVEA) -entities -I ../tex
IMAGEN=$(SETTEXINPUTS) imagen
PDFIMAGEN=pdfimagen
DVIPDFM=dvipdfm

all: manual.dvi manual.ps manual.pdf manual.html \
    splash.dvi splash.ps \
    scratch_write_splash.dvi scratch_write_splash.ps \
    scratch_draw_splash.dvi scratch_draw_splash.ps \
    mathmode.dvi \
    advi_remote.dvi advi_remote.html

clean::
	$(RM) $*.dvi *~ *.aux *.log manual.html *.dvi *.pdf *.pdfm *.ps \
		*.ima *.out *.toc *.haux *.htoc
	$(MAKE) -f MakeHTML clean

distribute: clean all web

web:
	$(MAKE) -f MakeHTML all

manual.dvi: manual.sty splash_contents.tex \
       scratch_write_splash_contents.tex scratch_draw_splash_contents.tex
splash.dvi: splash_contents.tex
scratch_write_splash.dvi: scratch_write_splash_contents.tex
scratch_draw_splash.dvi: scratch_draw_splash_contents.tex

ifeq ($(HAVE_HEVEA),true)
manual.html: manual.tex manual.sty macros.hva 
advi_remote.html: advi_remote.tex manual.sty macros.hva 
else
manual.html:
advi_remote.html:
endif

.SUFFIXES: .tex .dvi .advi .ps .pdf .pdfm .html .ima

.tex.dvi:
        # To avoid default hypertex.cfg if it exists.
        # Active-DVI really needs the hypertex driver...
	$(LATEX) '\def\driver{hypertex}\input' $<
	$(LATEX) '\def\driver{hypertex}\input' $<
	$(LATEX) '\def\driver{hypertex}\input' $<

.tex.ps:
	$(LATEX) '\def\driver{dvips}\input' $<
	$(DVIPS) -o $@ $*.dvi
	$(RM) $*.dvi

.tex.pdf:
	$(LATEX) '\def\driver{dvipdfm}\input' $<
	- $(DVIPDFM) -o $@ $*.dvi
	$(RM) $*.dvi

.advi.dvi:
	cp -p $< $*.dvi

.html.ima:
	if [ -f $*.image.tex ]; then $(PDFIMAGEN) $*; fi
	touch $*.ima

.tex.html:
	$(HEVEA) -fix macros.hva -o $*.html $*.tex
