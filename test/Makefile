# To view the demo, use ../advi demo.dvi
include ../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=.:/usr/share/texmf//:$$TEXINPUTS; export TEXINPUTS; latex

MLPROG=taquin

all: demo.dvi trigo_source.dvi ./trans.dvi trans2.dvi background.dvi \
     $(MLPROG) 

.tex.dvi:
	$(LATEX) $<

# but: start_embed.ml
# ifeq ($(HAVE_CDK),true)  
# 	  ocamlc -custom -o but `cdk_config camltk` start_embed.ml
# else
# 	  ocamlc -custom -o but -I +camltk camltk.cma start_embed.ml
# endif

taquin: taquin_embed.ml
ifeq ($(HAVE_CDK),true)
	ocamlc -custom -o taquin `cdk_config camltk` taquin_embed.ml
else
	ocamlc -custom -o taquin -I +camltk camltk.cma taquin_embed.ml
endif

# We do not remove jpdemo.dvi, because only Furuse can make it at Cristal.
clean:
	$(RM) a.out *.aux *.log demo.dvi trans.dvi trans2.dvi background.dvi *~
	$(RM) $(MLPROG) *.cmi *.cmo
	$(RM) .advi/*

test_subwindow:
	ocamlc -custom -I .. unix.cma graphics.cma -ccopt -L.. ../grY11.o ../events.o ../graphicsY11.cmo test_subwindow.ml 
