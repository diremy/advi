# To view the demo, use ../advi demo.dvi
include ../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:../tex:$(TEXINPUTS): latex
PLATEX=TEXINPUTS=.:../tex:$(TEXINPUTS): platex

TEXFILES= demo.tex demo2.tex trigo_source.tex \
trans.tex trans2.tex trans3.tex background.tex \
color.tex sleep.tex
#thumbnails.tex
DVIFILES= $(TEXFILES:%.tex=%.dvi)
ADVI=TEXPSHEADERS=../tex: ../advi

ifeq ($(HAVE_CAMLTK),true)
MLPROG=taquin adviclock
else
MLPROG=
endif

all: $(DVIFILES) $(MLPROG) 

$(DVIFILES): ../tex/bar.jpg.eps demo.sty ../tex/advi.sty ../tex/advi-annot.sty

.dvi.advi:
	$(ADVI) $<

.tex.dvi:
	if test "$<" = "jpdemo.tex" ; then $(PLATEX) $<; \
	else $(LATEX) $<; \
	fi

background.dvi: background.tex
	$(LATEX) $<; $(LATEX) $<;

taquin: taquin_embed.ml
ifeq ($(HAVE_CDK),true)
	ocamlc -custom -o taquin `cdk_config camltk` taquin_embed.ml
else
	ocamlc -custom -o taquin -I $(CAMLTKDIR) $(CAMLTKLIB) taquin_embed.ml
endif

adviclock: adviclock.ml
ifeq ($(HAVE_CDK),true)
	ocamlc -custom -o adviclock `cdk_config camltk` adviclock.ml
else
	ocamlc -custom -o adviclock unix.cma -I $(CAMLTKDIR) $(CAMLTKLIB) adviclock.ml
endif

clean::
	$(RM) a.out *.aux *.log *.dvi *~ 
	$(RM) $(MLPROG) *.cmi *.cmo
	$(RM) .advi/*

test_subwindow:
	ocamlc -custom -I .. unix.cma graphics.cma -ccopt -L.. ../grY11.o ../events.o ../graphicsY11.cmo test_subwindow.ml 

trigo.tex: trigo.pic
	gpic -t trigo.pic > trigo.tex

test_gpic.tex: test_gpic.pic
	gpic -t test_gpic.pic > test_gpic.tex
clean::
	$(RM) test_gpic.tex

.SUFFIXES: .tex .sty .dvi .ps .eps .advi
