TEXINPUTS := ../../lib:../../../tex:$(TEXINPUTS)
ADVI=../../../advi.opt
LATEX=TEXINPUTS=$(TEXINPUTS) latex 

show: ll.dvi
	$(ADVI) ll.dvi

ll.dvi: ll.tex world.jpg advilogo.anim.gif
	$(LATEX) ll.tex


clean:
	rm -f ll.{aux,log,dvi}


.phony: show

