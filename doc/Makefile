TEXINPUTS := ../tex/:$(TEXINPUTS)
all: manual.ps manual.pdf 

manual.dvi: manual.tex
	latex $<

manual.ps: manual.tex
	latex '\def\driver{dvips}\input' $<
	dvips -o $@ $*.dvi

manual.html: manual.tex
	hevea -o $*.html $*.tex
	if [ -f $<.image.tex ]; then mv $<.image.tex $*.image.tex; fi
	if [ -f $*.image.tex ]; then imagen $*; fi

manual.ima: manual.html
	if [ -f $*.image.tex ]; then pdfimagen $*; fi
	touch manual.ima

manual.pdf: manual.tex
	latex '\def\driver{dvipdfm}\input' $<
	dvipdfm -o $@ $*.dvi

clean: 
	rm -f manual.{aux,log,html,tex,dvi,pdf,pdfm,ps,eps,ima,out} \
	  manual.image.* manual0[0-9][0-9].{eps,gif,pdf} 

.SUFFIXES: .tex .dvi .ps .pdf .pdfm .tex .html .out .ima
