# advi - A DVI previewer
# Copyright (C) 2000 Alexandre Miquel
# Copyright (C) 2001 Alexandre Miquel, Jun Furuse, Xavier Leroy, Didier Rémy,
#		     Alan Schmitt, Pierre Weis
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

MODULES	  = config timeout graphicsY11 misc options launch \
            userfile input symbol search \
            shot drawimage dvicolor \
	    table pkfont ttfont jfm font glyph devfont \
	    units dimension \
	    gs transimpl grdev dvi driver dviview main
LIBRARIES = graphics unix str $(CAMLIMAGESLIBS)
CLIBS	  = graphics unix str

COBJS     = events.o grY11.o

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
CFLAGS=$(X11_INCLUDES) -O $(BYTECCCOMPOPTS)

byte: $(EXEC)
opt: $(EXEC).opt 
allopt: opt
all: byte opt

$(EXEC): $(COBJS) $(CMO_OBJS)
	$(OCAMLC) -custom $(INCLUDES) $(BYTE_OBJS) $(LINK_OPTS) -o $(EXEC)

$(EXEC).opt: $(COBJS) $(CMX_OBJS)
	$(OCAMLOPT) $(INCLUDES) $(OPT_OBJS) $(LINK_OPTS) -o $(EXEC).opt

config.ml: config.ml.in
	configure
 
dvicolor.ml: Makefile.config dvicolor.mlp ifdef.cmo
	camlp4o pa_ifdef.cmo ./ifdef.cmo -impl dvicolor.mlp > $@

drawimage.ml: Makefile.config drawimage.mlp ifdef.cmo
	camlp4o pa_ifdef.cmo ./ifdef.cmo -impl drawimage.mlp > $@

ttfont.ml: Makefile.config ttfont.mlp ifdef.cmo
	camlp4o pa_ifdef.cmo ./ifdef.cmo -impl ttfont.mlp > $@

ifdef.cmo: ifdef.ml
	$(OCAMLC) -c $(CAMLP4_FLAG) $<

grY11.o : grY11.c
	$(OCAMLC) -ccopt "$(CFLAGS)" -c $<

events.o : events.c
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
	cd test; $(MAKE) clean

veryclean: clean
	rm -f Makefile.config config.cache config.log \
	config.status drawps.ml ttfont.ml config.ml ifdef.ml

veryveryclean: veryclean
	rm -f configure

installopt:install
install:: advi.opt tex/splash.dvi
	cp advi.opt ${bindir}/advi
	- mkdir -p $(ADVI_LOC)
	cp tex/splash.dvi tex/advilogo.eps tex/caml.eps tex/bar.jpg.eps tex/*.sty tex/advi.pro $(ADVI_LOC)
	if [ -f conf/jpfonts.conf ]; then cp conf/jpfonts.conf $(ADVI_LOC); fi

MLFILES = $(addsuffix .ml, $(MODULES))
.depend:: *.mli $(MLFILES) Makefile 
	$(OCAMLDEP) *.mli $(MLFILES) > .depend
	gcc -MM -I$(CAMLDIR) $(CFLAGS) $(COBJS:.o=.c) | sed -e 's|$(CAMLDIR)/[^ ]*||' >> .depend

# just for the authors
VERSION=1.0.0
ADVI=advi-$(VERSION)
WEBSITEDIR=/net/pauillac/infosystems/www/advi
FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal/advi

# make splash builds splash.dvi
tex/splash.dvi:
	cd tex; latex splash.tex

distribute: tar_and_web rpm

tar_and_web: tex/splash.dvi
	(cd test; make all jpdemo.dvi)
	rm -rf release
	rm -rf $(WEBSITEDIR)/*
	- mkdir $(WEBSITEDIR)
	mkdir release
	cd release; cvs co bazar-ocaml/advi; \
	cp -p ../test/*.dvi bazar-ocaml/advi/test/; \
	cp -p ../tex/splash.dvi bazar-ocaml/advi/tex/; \
	cp -p ../tex/advi.pro bazar-ocaml/advi/test/; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	find . -name 'advi-development-kit' -print | xargs rm -rf; \
	cp -pr bazar-ocaml/advi/doc/* $(WEBSITEDIR)
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)
	cp -pr release/bazar-ocaml/advi/LGPL release/bazar-ocaml/advi/README release/bazar-ocaml/advi/INDEX $(FTPSITEDIR)
	cd release; mv bazar-ocaml/advi $(ADVI); \
	tar cvf $(ADVI).tar $(ADVI); \
	gzip $(ADVI).tar; \
	mv -f $(ADVI).tar.gz $(FTPSITEDIR)
	rm -rf release
	cd advi-development-kit; make distribute

rpm:
	if test -d /usr/src/redhat; then rpmdir=/usr/src/redhat; \
	else if test -d /usr/src/RPM; then rpmdir=/usr/src/RPM; fi; fi; \
	if test "X$$rpmdir" = "X"; then \
		echo "cannot create rpm"; exit 2; fi; \
	echo YOU NEED TO SU ROOT; \
	su root -c "$(FTPSITEDIR)/$(ADVI).tar.gz $$rpmdir/SOURCES/; rpm -ba --clean ./advi.spec"; \
	cp $$rpmdir/SRPMS/advi-$(VERSION)-1.src.rpm \
	   $$rpmdir/RPMS/*/advi-$(VERSION)-1.*.rpm $(FTPSITEDIR)

include .depend
