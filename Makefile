# advi - A DVI previewer
# Copyright (C) 2000 Alexandre Miquel
# Copyright (C) 2001 Alexandre Miquel, Jun Furuse, Xavier Leroy, Didier R�my,
#		     Alan Schmitt
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# See the GNU Lesser General Public License version 2.1 for more
# details (enclosed in the file LGPL).

include Makefile.config

COPTIONS = -g
COPTOPTIONS = -unsafe -inline 9

OCAMLC	  = $(CAML)c $(COPTIONS)
OCAMLOPT  = $(CAML)opt $(COPTOPTIONS)
# OCAMLOPT  = $(CAML)opt.opt -unsafe -inline 9 -ccopt -I
OCAMLDEP  = $(CAML)dep

# CAMLIMAGESDIR & CAMLIMAGESLIBS is defined in Makefile.config

MLINCDIRS = $(CAMLIMAGESDIR)

EXEC	  = advi

MODULES	  = config misc input symbol search \
	    graphicsY11 drawps draw_image dvicolor \
	    table pkfont ttfont jfm font glyph devfont \
	    units dimension \
	    gs transimpl grdev dvi driver dviview main
LIBRARIES = graphics unix str $(CAMLIMAGESLIBS)
CLIBS	  = unix graphics

COBJS	  = grY11.o

CMO_OBJS  = $(addsuffix .cmo, $(MODULES))
CMX_OBJS  = $(addsuffix .cmx, $(MODULES))

ifeq ($(HAVE_CDK),true)
CMA_OBJS  = `cdk_config $(LIBRARIES)` 
CMXA_OBJS = `cdk_config -opt $(LIBRARIES)`
CAMLP4_FLAG = `cdk_config -c camlp4`
else
CMA_OBJS  = $(addsuffix .cma, $(LIBRARIES))
CMXA_OBJS = $(addsuffix .cmxa, $(LIBRARIES))
CAMLP4_FLAG = -I +camlp4
endif

BYTE_OBJS = $(COBJS) $(CMA_OBJS) $(CMO_OBJS)
OPT_OBJS  = $(COBJS) $(CMXA_OBJS) $(CMX_OBJS)

INCLUDES  = $(addprefix -I , $(MLINCDIRS))
LINK_OPTS = $(addprefix -ccopt -L, $(CLIBDIRS)) \
	    $(addprefix -cclib -l, $(CLIBS)) \
	    -cclib "$(WITH_X)"

X11_INCLUDES=-I/usr/X11R6/include
BYTECCCOMPOPTS=-fno-defer-pop -Wall -Wno-unused
#CFLAGS=-I$(CAMLDIR)/caml $(X11_INCLUDES) -O $(BYTECCCOMPOPTS)
CFLAGS=-I$(CAMLDIR)/caml $(X11_INCLUDES) -O $(BYTECCCOMPOPTS)

byte: $(EXEC)
opt: $(EXEC).opt 
allopt: opt
all: byte opt

$(EXEC): $(COBJS) $(CMO_OBJS)
	$(OCAMLC) -custom $(INCLUDES) $(BYTE_OBJS) $(LINK_OPTS) -o $(EXEC)

$(EXEC).opt: $(COBJS) $(CMX_OBJS)
	$(OCAMLOPT) $(INCLUDES) $(OPT_OBJS) $(LINK_OPTS) -o $(EXEC).opt

# config.ml:
#	configure
 
dvicolor.ml: Makefile.config dvicolor.mlp ifdef.cmo
	camlp4o pa_ifdef.cmo ./ifdef.cmo -impl dvicolor.mlp > $@

drawps.ml: Makefile.config drawps.mlp ifdef.cmo
	camlp4o pa_ifdef.cmo ./ifdef.cmo -impl drawps.mlp > $@

ttfont.ml: Makefile.config ttfont.mlp ifdef.cmo
	camlp4o pa_ifdef.cmo ./ifdef.cmo -impl ttfont.mlp > $@

ifdef.cmo: ifdef.ml
	$(OCAMLC) -c $(CAMLP4_FLAG) $<

grY11.o : grY11.c
	$(OCAMLC) -ccopt "$(CFLAGS)" -c $<

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) $(INCLUDES) -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -c $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c $<

count:
	wc -l *.ml *.mli | sort -n

clean:
	rm -f *.cm[oix] *.o $(EXEC) $(EXEC).opt *~ .depend *.log *.aux
	rm -rf .advi test/.advi test/*.log test/*.aux test/*.dvi \
	tex/.advi tex/*.log tex/*.aux tex/*.dvi

veryclean: clean
	rm -f Makefile.config config.cache config.log \
	config.status drawps.ml ttfont.ml config.ml ifdef.ml

veryveryclean: veryclean
	rm -f configure

tex/splash.dvi: tex/splash.tex
	cd tex; latex splash.tex

test/demo.dvi: test/demo.tex
	cd test; $(MAKE) demo.dvi

test/trans.dvi: test/trans.tex
	cd test; $(MAKE) trans.dvi

install:: advi.opt tex/splash.dvi
	cp advi.opt ${bindir}/advi
	- mkdir -p $(ADVI_LOC)
	cp tex/splash.dvi tex/advilogo.eps tex/caml.eps tex/bar.jpg.eps tex/advi.sty tex/advi.pro $(ADVI_LOC)

MLFILES = $(addsuffix .ml, $(MODULES))
.depend:: *.mli $(MLFILES) Makefile 
	$(OCAMLDEP) *.mli $(MLFILES) > .depend
	gcc -MM $(CFLAGS) $(COBJS:.o=.c) | sed -e 's|$(CAMLDIR)/caml/[^ ]*||' >> .depend

# just for the authors
VERSION=0.4.0
ADVI=advi-$(VERSION)
WEBSITEDIR=/net/pauillac/infosystems/www/advi

# make splash builds splash.dvi
splash:
	cd tex; latex splash.tex

distribute: tex/splash.dvi test/demo.dvi test/trans.dvi	
	rm -rf release
	rm -rf $(WEBSITEDIR)/*
	- mkdir $(WEBSITEDIR)
	mkdir release
	cd release; cvs co bazar-ocaml/advi; \
	cp -p ../test/demo.dvi bazar-ocaml/advi/test/; \
	cp -p ../tex/splash.dvi bazar-ocaml/advi/tex/; \
	cp -p ../tex/advi.pro bazar-ocaml/advi/test/; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	cp -pr bazar-ocaml/advi/doc/* $(WEBSITEDIR)
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)
	cd release; mv bazar-ocaml/advi $(ADVI); \
	tar cvf $(ADVI).tar $(ADVI); \
	gzip $(ADVI).tar; \
	mv -f $(ADVI).tar.gz $(WEBSITEDIR)
	rm -rf release

include .depend
