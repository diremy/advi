# To view the demo, use ../advi file.dvi

SUBDIRS=cash

TEXFILES= simple.tex moderate.tex semi-complex.tex

ADVIDIR=../..

include ../../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): latex
PLATEX=TEXINPUTS=.:$(ADVIDIR)/tex:$(TEXINPUTS): platex

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
	for i in $(SUBDIRS); do \
	  cd $$i; \
	  $(MAKE) clean;  \
	done

.SUFFIXES: .tex .sty .dvi .eps .advi
