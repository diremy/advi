TEXINPUTS := .:$(TOPDIR)/examples/lib:$(TOPDIR)/tex:
ADVI=TEXPSHEADERS=.:$(TOPDIR)/tex: $(TOPDIR)/advi.opt
LATEX=TEXINPUTS=$(TEXINPUTS) latex 
STY=advi.sty advi-annot.sty bubble.sty 
LIBSTY=commandline.sty xprosper.sty
DEPENDS=$(addprefix $(TOPDIR)/tex/, $(STY)) \
        $(addprefix $(TOPDIR)/examples/lib/, $(LIBSTY))
DVIFILES=$(FILES:%.tex=%.dvi) Makefile

all:: $(DVIFILES)

.dvi.advi:
	$(ADVI) $<

.tex.dvi: 
	$(LATEX) $*

.tex.ps:
	$(LATEX) '\def\argv{advi=ignore,bubble=ps,hyperref=dvips}\input' $*
	dvips -o $@ $*.dvi

.tex.pdf:
	$(LATEX) '\def\argv{advi=ignore,hyperref=dvipdfm,xprosper=dvipdfm}\input' $*
	dvipdfm -o $@ $*.dvi

clean:
	rm -f $(FILES:%.tex=%.{aux,log,dvi,ps,pdf})

.SUFFIXES: .tex .dvi .advi .ps .pdf
