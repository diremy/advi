TOPDIR=../../..
FILES=tools.tex 
include ../../lib/Makefile

tools.dvi: tools.tex
	ln -sf ../../../tex/advi.pro advi.pro
	$(LATEX) $<

tools.pdf: tools.tex
	$(LATEX) '\def\argv{ps2pdf}\input' $<
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi 

.phony: show

