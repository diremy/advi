# To view the demo, use ../advi demo.dvi
ADVIDIR=../..

include ../../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): latex
PLATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): platex

TEXFILES= simple.tex
DVIFILES= $(TEXFILES:.tex=.dvi)
ADVI=TEXPSHEADERS=$(ADVIDIR)/tex: $(ADVIDIR)/advi

MLPROG=

all: $(DVIFILES) $(MLPROG) 

$(DVIFILES): $(ADVIDIR)/tex/bar.jpg.eps $(ADVIDIR)/tex/advi.sty $(ADVIDIR)/tex/advi-annot.sty

.dvi.advi:
	$(ADVI) $<

.tex.dvi:
	$(LATEX) $<

clean:
	$(RM) a.out *.aux *.log *.dvi *~ 
	$(RM) $(MLPROG) *.cmi *.cmo
	$(RM) .advi/*

.SUFFIXES: .tex .sty .dvi .eps .advi
