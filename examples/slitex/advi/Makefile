LATEX=latex

#OSTYLES=~/lib/tex/STYLES

.SUFFIXES: .tex

all: talk
	advi talk.dvi

clean:
	/bin/rm -f *.z[io] *~ *.txt *.log *.aux *.dvi spelling #*#

scratch: clean
	/bin/rm -f *.dvi; rm -f *.bbl

talk: talk.tex slides.tex
	@ TEXINPUTS=$(OSTYLES):$$TEXINPUTS; export TEXINPUTS; latex talk.tex

ps:
	dvips -o mod_talk.ps -t landscape talk.dvi
