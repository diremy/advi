LATEX=latex

#OSTYLES=~/lib/tex/STYLES

.SUFFIXES: .tex

all: talk
	advi talk.dvi

clean:
	/bin/rm -f *.z[io] *~ *.txt *.log *.aux *.dvi spelling #*#

scratch: clean
	/bin/rm -f *.dvi; rm -f *.bbl

talk: talk.tex

.tex.dvi:
	@ TEXINPUTS=$(OSTYLES):$$TEXINPUTS; export TEXINPUTS; latex $<

ps:
	dvips -o mod_talk.ps -t landscape talk.dvi
