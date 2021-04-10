CONFIG = ../_build/default/src/Makefile.config

TEXINPUTS = .:../tex/:

TEX = $(shell ls *.tex)
STY = $(shell ls ../tex/*.sty)
EPS = $(shell ls ../tex/*.eps)

DEPS = $(TEX) $(STY) $(EPS)

OUTDIR = _build

LATEX = TEXINPUTS=$(TEXINPUTS) latexmk -outdir=$(OUTDIR)

MANFILE = advi.1
SPLASHFILES = splash.dvi scratch_draw_splash.dvi scratch_write_splash.dvi 

DISTFILES = $(MANFILE) $(SPLASHFILES)
DISTBUILD = $(patsubst %, $(OUTDIR)/%, $(DISTFILES))

MANUALFILES = manual.pdf manual.html manualxxx.html
MANUALBUILD = $(patsubst %, $(OUTDIR)/%, $(MANUALFILES))


.PHONY: all manual distbuild distclean promote clean install install.manual uninstall

all: $(DISTFILES)

manual: $(MANUALBUILD)

$(OUTDIR)/.exists:
	mkdir -p $(OUTDIR)
	touch $(@)


$(CONFIG): 
	make -C .. src/Makefile.config

include $(CONFIG)

$(OUTDIR)/%.dvi: %.tex $(DEPS)
	$(LATEX) -dvi $<

$(OUTDIR)/manual.pdf: manual.tex $(DEPS)
	$(LATEX) -ps $<
	ps2pdf $(OUTDIR)/manual.ps $@

$(OUTDIR)/manual.html: manual.tex manual.hva $(DEPS) $(OUTDIR)/.exists
	hevea -entities -I . -I ../tex -I $(OUTDIR) -o $@ -fix manual.hva $<

$(OUTDIR)/manualxxx.html: manual.html 
	cd $(OUTDIR) && hacha -o manualxxx.html manual.html

$(OUTDIR)/advi.1: advi.md  $(OUTDIR)/.exists
	pandoc --standalone --from markdown $< --to man --output $@

distbuild: $(DISTBUILD)

distclean:
	rm -f $(DISTFILES)

promote: $(DISTBUILD)
	for i in $(DISTFILES); do ln -f $(OUTDIR)/$$i .; done

clean:  
	rm -rf _build


install: $(MANFILE) $(SPLASHFILES)
	install -d $(advi_mandir)/man1/ $(advi_docdir)/
	install $(MANFILE) $(advi_mandir)/man1/
	install $(SPLASHFILES) $(advi_docdir)/

install.manual: $(MANUALBUILD)
	install -d $(advi_docdir)/
	install $(OUTDIR)/manual.pdf $(OUTDIR)/manual.html $(advi_docdir)/
	install $(OUTDIR)/manualxxx.html $(advi_docdir)/index.html
	install _build/manual???.html $(advi_docdir)/

uninstall: 
	rm -vf $(advi_mandir)/man1/advi.1
	rm -vf $(advi_docdir)/index.html 
	rm -vf $(advi_docdir)/manual???.html 
	for i in $(DOCTARGETS); do rm -vf $(advi_docdir)/$$i; done

