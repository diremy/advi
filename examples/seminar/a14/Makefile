N= 14

all: a${N}.dvi
	advi a${N}.dvi

a${N}.dvi: mes a${N}.tex
	./putimage <a${N}.tex | gpic -t > a${N}bis.tex
	- latex a${N}bis.tex
	mv -f a${N}bis.dvi a${N}.dvi

mes:
	echo "" > mon-dvipdf.tex
	${MAKE} clear

ps: a${N}.tex a${N}.dvi
	dvips -f a${N}.dvi >a${N}.ps

light:
	echo "\usepackage{colors-light}" > mon-color.tex

clear:
	echo "\usepackage{colors-clear}" > mon-color.tex

dark:
	echo "\usepackage{colors-dark}" > mon-color.tex

html: mes
	./putimage <a${N}.tex |gpic -t > a${N}bis.tex
	hevea sem.hva a${N}bis.tex
	${MAKE} images
	mv -f a${N}bis.html a${N}.html

images:
	imagen -extra "pnmflip -ccw" -mag 1700 a${N}bis

pdf:
	echo "\usepackage[dvipdfm]{hyperref}" >mon-dvipdf.tex
	/bin/rm -f a${N}.dvi
	${MAKE} a${N}.dvi
	dvipdfm a${N}.dvi 

PUBLIC_HTML=ma-machine:/usr/home/moi/public_html

web: html
	scp a${N}.html a${N}.ps a${N}.pdf a${N}*.gif ${PUBLIC_HTML}/cours/a${N}

clean:
	/bin/rm -f *~ *.dvi *.ps *.pdf *.html \
        *.log *.aux *.out *.gif \
	a${N}bis.tex a${N}bis.image.tex mon-color.tex mon-dvipdf.tex
