TEXINPUTS := .:../lib:../../tex:
ADVI=TEXPSHEADERS=../../tex: ../../advi.opt
LATEX=TEXINPUTS=$(TEXINPUTS) latex 
DEP=$(addprefix ../../tex/, advi.sty advi-annot.sty)

show: join.dvi  
	$(ADVI) join.dvi

join.dvi: join.tex $(DEP)
	$(LATEX) join.tex


clean:
	rm -f join.{aux,log,dvi}


.phony: show

