# To view the demo, use ../advi demo.dvi
include ../Makefile.config

RM=/bin/rm -f

LATEX=TEXINPUTS=/usr/share/texmf//:$$TEXINPUTS; export TEXINPUTS; latex

MLPROG=but taquin

all: $(MLPROG) demo.dvi trans.dvi

demo.dvi: demo.tex
	$(LATEX) ./demo.tex

trans.dvi: trans.tex
	$(LATEX) ./trans.tex

but: start_embed.ml
ifeq ($(HAVE_CDK),true)  
	ocamlc -custom -o but `cdk_config camltk` start_embed.ml
else
	ocamlc -custom -o but -I +camltk camltk.cma start_embed.ml
endif

taquin: taquin_embed.ml
ifeq ($(HAVE_CDK),true)
	ocamlc -custom -o taquin `cdk_config camltk` taquin_embed.ml
else
	ocamlc -custom -o taquin -I +camltk camltk.cma taquin_embed.ml
endif

clean:
	$(RM) a.out *.aux *.log *.dvi *~
	$(RM) $(MLPROG) *.cmi *.cmo
