# To view the demo, use ../advi demo.dvi
RM=/bin/rm -f

LATEX=TEXINPUTS=/usr/share/texmf//:$$TEXINPUTS; export TEXINPUTS; latex

MLPROG=but taquin

all: $(MLPROG)
	$(LATEX) ./demo.tex

but: start_embed.ml
	ocamlc -custom -o but -I +camltk camltk.cma start_embed.ml

taquin: taquin_embed.ml
	ocamlc -custom -o taquin -I +camltk camltk.cma taquin_embed.ml

clean:
	$(RM) a.out *.aux *.log *.dvi *~
	$(RM) $(MLPROG) *.cmi *.cmo
