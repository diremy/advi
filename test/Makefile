# To view the demo, use ../advi demo.dvi
include ../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:../tex:$(TEXINPUTS): latex
PLATEX=TEXINPUTS=.:../tex:$(TEXINPUTS): platex

TEXFILES= demo.tex trigo_source.tex trans.tex trans2.tex background.tex
DVIFILES= $(TEXFILES:%.tex=%.dvi)

ifeq ($(HAVE_CAMLTK),true)
MLPROG=taquin
else
MLPROG=
endif

all: $(DVIFILES) $(MLPROG) 

$(DVIFILES): ../tex/bar.jpg.eps demo.sty

.tex.dvi: 
	if test "$<" = "jpdemo.tex" ; then $(PLATEX) $<; \
	else $(LATEX) $<; \
	fi 

taquin: taquin_embed.ml
ifeq ($(HAVE_CDK),true)
	ocamlc -custom -o taquin `cdk_config camltk` taquin_embed.ml
else
	ocamlc -custom -o taquin -I $(CAMLTKDIR) camltk.cma taquin_embed.ml
endif

clean:
	$(RM) a.out *.aux *.log *.dvi *~ 
	$(RM) $(MLPROG) *.cmi *.cmo
	$(RM) .advi/*

test_subwindow:
	ocamlc -custom -I .. unix.cma graphics.cma -ccopt -L.. ../grY11.o ../events.o ../graphicsY11.cmo test_subwindow.ml 

.SUFFIXES: .tex .sty .dvi .eps
