TEXINPUTS := ../lib:../../tex:../../lib:$(TEXINPUTS)
ADVI=../../advi.opt

show: join.dvi
	$(ADVI) join.dvi

join.dvi: join.tex
	latex join.tex


clean:
	rm -f join.{aux,log,dvi}


.phony: show

