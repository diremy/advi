CONFIG = ../_build/default/src/Makefile.config

SOURCES = \
  advi-annot.sty \
  advi-graphicx.sty \
  advi-slides.sty \
  advi.hva \
  advi.sty \
  argv.sty \
  bubble.sty \
  pgfsys-dvips.def \
  superpose.sty \
  xwindows-colors.sty \
  caml.eps \
  bar.eps \
  advilogo.eps

OLDSOURCES =  advi.pro

all: $(TARGETS)

$(CONFIG):
	(cd ../src && dune build Makefile.config)

include $(CONFIG)

clean:  
	rm -rf $(TARGETS) _build

install: $(SOURCES) 
	install -d $(advi_texdir)/tex/latex/
	install  $(SOURCES) $(advi_texdir)/tex/latex/


