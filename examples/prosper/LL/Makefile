TEXINPUTS := ..:../../lib:../../../tex:$(TEXINPUTS)
ADVI=../../../advi.opt
LATEX=TEXINPUTS=$(TEXINPUTS) latex 

show: ll.dvi
	$(ADVI) ll.dvi

ll.dvi: ll.tex world.jpg advilogo.anim.gif
	$(LATEX) $<

ll.pdf: ll.tex
	$(LATEX) '\def\argv{ps2pdf}\input' $<
	dvips -t landscape -o ll.ps ll.dvi && ps2pdf ll.ps ll.pdf
	./pdfrotate.pl ll.pdf
	rm ll.dvi

clean:
	/bin/rm -f ll.aux ll.log ll.dvi


.phony: show

