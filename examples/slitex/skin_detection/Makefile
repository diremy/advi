all: skin_detection.dvi see

edit:
	emacs skin_detection.tex

see:
	advi skin_detection.dvi &

clean:
	rm -f *~ *.log *.dvi

.SUFFIXES: .tex .dvi

.tex.dvi:
	latex $<

skin_detection.dvi: skin_detection.tex



