TOPDIR=../../..
FILES=join.tex 
include ../../lib/Makefile

all: show

show: join.dvi
	$(ADVI) join.dvi

join.dvi: join.tex
	$(LATEX) $<

join.pdf: join.tex
	$(LATEX) '\def\argv{ps2pdf}\input' $<
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi

.phony: show

