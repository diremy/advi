# advi - A DVI previewer
# Copyright (C) 2000 Alexandre Miquel
# Copyright (C) 2001 Alexandre Miquel, Jun Furuse, Xavier Leroy, Didier Rémy
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

OCAMLC    = $(CAML)c $(COPTIONS)
OCAMLOPT  = $(CAML)opt $(COPTOPTIONS)
# OCAMLOPT  = $(CAML)opt.opt -unsafe -inline 9 -ccopt -I
OCAMLDEP  = $(CAML)dep

# CAMLIMAGESDIR & CAMLIMAGESLIBS is defined in Makefile.config

MLINCDIRS = $(CAMLIMAGESDIR)

EXEC      = advi
MODULES   = config misc search \
	    graphicsY11 drawps \
            table pkfont font glyph devfont dvi \
	    units dimension driver\
	    gs grdev dviview main
LIBRARIES = graphics unix $(CAMLIMAGESLIBS)
CLIBS     = unix graphics

COBJS     = grY11.o

CMO_OBJS  = $(addsuffix .cmo, $(MODULES))
CMX_OBJS  = $(addsuffix .cmx, $(MODULES))
CMA_OBJS  = $(addsuffix .cma, $(LIBRARIES))
CMXA_OBJS = $(addsuffix .cmxa, $(LIBRARIES))

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
# 	configure
 
drawps.ml: Makefile.config drawps_with_ps.ml drawps_without_ps.ml
	rm -f drawps.ml
	if [ $(HAVE_CAMLIMAGES) = "true" -a $(HAVE_GS) = "true" ]; then \
		cp drawps_with_ps.ml drawps.ml; \
	else \
		cp drawps_without_ps.ml drawps.ml; \
	fi

drawps.cmo : drawps.cmi

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
	rm -f *.cm[oix] *.o $(EXEC) $(EXEC).opt *~ .depend *.{log,aux}
	rm -rf .advi test/.advi

veryclean:
	rm -f Makefile.config config.cache config.log *.dvi \
	config.status drawps.ml config.ml

splash.dvi: splash.tex
	latex $*

install:: advi.opt splash.dvi
	cp advi.opt ${bindir}/advi
	- mkdir -p $(ADVI_LOC)
	cp tex/splash.dvi tex/caml.eps tex/bar.jpg.eps tex/advi.sty tex/advi-annot.sty tex/advi.pro $(ADVI_LOC)

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

distribute:
	rm -rf release
#	rm -rf $(WEBSITEDIR)/*
	- mkdir $(WEBSITEDIR)
	mkdir release
	cd release; cvs co bazar-ocaml/advi; \
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
