TEXINPUTS := ../lib:../../tex:../../lib:$(TEXINPUTS)
ADVI=../../advi.opt
LATEX=TEXINPITS=$(TEXINPUTS) latex 

show: join.dvi
	$(ADVI) join.dvi

join.dvi: join.tex
	$(LATEX) join.tex


clean:
	rm -f join.{aux,log,dvi}


.phony: show

