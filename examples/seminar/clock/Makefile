TOPDIR=../../..
FILES=tools.tex

include ../../lib/Makefile

all::
	advi tools.dvi

EPS=bar.jpg.eps cameleon.eps cameleon2.eps hump.eps logo.gif.eps ocaml.eps ocamldoc.eps ODBC.eps

tools.dvi: uncompress $(EPS) tools.tex
	ln -sf ../../../tex/advi.pro advi.pro
	$(LATEX) tools.tex

tools.pdf: $(EPS) tools.tex
	$(LATEX) '\def\argv{ps2pdf}\input' tools.tex
	dvips -o $*.ps $*.dvi && ps2pdf $*.ps $*.pdf
	rm $*.dvi 

uncompress:
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

