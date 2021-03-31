CONFIG = ../_build/default/src/Makefile.config

TEXINPUTS = .:../tex/:

TEX = $(shell ls *.tex)
STY = $(shell ls ../tex/*.sty)
EPS = $(shell ls ../tex/*.eps)

DEPS = $(TEX) $(STY) $(EPS)

OUTDIR = _build

LATEX= latexmk -outdir=$(OUTDIR)

MANTARGETS = advi.1
DOCTARGETS = splash.dvi manual.pdf manual.html 

TARGETS = $(MANTARGETS) $(DOCTARGETS) manualxxx.html

all: $(TARGETS)

$(CONFIG):
	(cd ../src && dune build src/Makefile.config)

include $(CONFIG)

splash.dvi: splash.tex $(DEPS)
	$(LATEX) -dvi $<
	ln -f $(OUTDIR)/$@ .

manual.pdf: manual.tex $(DEPS)
	$(LATEX) -ps $<
	ps2pdf $(OUTDIR)/manual.ps $(OUTDIR)/$@
	ln -f $(OUTDIR)/$@ .

manual.html: manual.tex manual.hva $(DEPS)
	hevea -entities -I . -I ../tex -I $(OUTDIR) -o $(OUTDIR)/$@ -fix manual.hva $<
	ln -f $(OUTDIR)/$@ .

manualxxx.html: manual.html
	cd $(OUTDIR) && hacha -o manualxxx.html manual.html
	ln -f $(OUTDIR)/$@ .

advi.1: advi.md
	pandoc --standalone --from markdown $< --to man --output $@

clean:  
	rm -rf $(TARGETS) _build

install: $(DOCTARGETS) $(MANTARGETS)
	install -d $(advi_mandir)/man1/ $(advi_docdir)/
	install $(MANTARGETS) $(advi_mandir)/man1/
	install $(DOCTARGETS) $(advi_docdir)/

uninstall: 
	rm -vf $(advi_mandir)/man1/advi.1
	for i in $(DOCTARGETS); do rm -vf $(advi_docdir)/$$i; done

