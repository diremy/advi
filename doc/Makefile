LATEX=TEXINPUTS=.:../tex: latex
DVIPS=TEXPSHEADERS=.:../tex: dvips
HEVEA=hevea
IMAGEN=imagen
PDFIMAGEN=pdfimagen
DVIPDFM=dvipdfm

all: manual.ps manual.pdf splash.dvi splash.ps mathmode.dvi

clean: 
	rm -f *.aux *.log manual.html *.dvi *.pdf *.pdfm *.ps *.ima *.out *.toc

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

