# To view the demo, use ../advi demo.dvi
include ../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:../tex:/usr/share/texmf//:$$TEXINPUTS; export TEXINPUTS; latex
PLATEX=TEXINPUTS=.:/usr/share/texmf//:$$TEXINPUTS; export TEXINPUTS; platex

ifeq ($(HAVE_CAMLTK),true)
MLPROG=taquin
else
MLPROG=
endif

all: demo.dvi trigo_source.dvi trans.dvi trans2.dvi background.dvi \
     $(MLPROG) 

.tex.dvi: ../tex/bar.jpg.eps
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
