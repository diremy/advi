TOPDIR=../../..
FILES=tools.tex
include ../../lib/Makefile

EPS=bar.jpg.eps cameleon.eps cameleon2.eps hump.eps logo.gif.eps ocaml.eps ocamldoc.eps ODBC.eps

tools.dvi: $(EPS) tools.tex
	ln -sf ../../../tex/advi.pro advi.pro
	$(LATEX) tools.tex

tools.pdf: $(EPS) tools.tex
	$(LATEX) '\def\argv{ps2pdf}\input' tools.tex
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi 

.SUFFIXES: .eps_gz .eps

clean:
	rm -f *.eps *.dvi *.pdf *.aux *.log *.ps *~

%.eps:%.eps.gz
	gunzip -c $< > $@

.phony: show

