LATEX=TEXINPUTS=.:../tex: latex
DVIPS=TEXINPUTS=.:../tex: dvips

all: manual.ps manual.pdf splash.dvi splash.ps

manual.dvi: manual.tex
	$(LATEX) $<

manual.ps: manual.tex
	$(LATEX) '\def\driver{dvips}\input' $<
	$(DVIPS) -o $@ $*.dvi

manual.html: manual.tex
	hevea -o $*.html $*.tex
	if [ -f $<.image.tex ]; then mv $<.image.tex $*.image.tex; fi
	if [ -f $*.image.tex ]; then imagen $*; fi

manual.ima: manual.html
	if [ -f $*.image.tex ]; then pdfimagen $*; fi
	touch manual.ima

manual.pdf: manual.tex
	$(LATEX) '\def\driver{dvipdfm}\input' $<
	- dvipdfm -o $@ $*.dvi

splash.dvi: splash.tex
	$(LATEX) $<

splash.ps: splash.tex
	$(LATEX) '\def\driver{dvips}\input' $<
	$(DVIPS) -o $@ $*.dvi

clean: 
	rm -f *.aux *.log manual.html *.dvi *.pdf *.pdfm *.ps *.ima *.out *.toc

.SUFFIXES: .tex .dvi .ps .pdf .pdfm .tex .html .out .ima
