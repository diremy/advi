#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier R�my and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

include Makefile.config

VERSION=1.2

COPTIONS = -warn-error A -g
COPTOPTIONS = -warn-error A -unsafe -inline 9

OCAMLC	  = $(CAML)c $(COPTIONS)
OCAMLOPT  = $(CAML)opt $(COPTOPTIONS)
# OCAMLOPT  = $(CAML)opt.opt -unsafe -inline 9 -ccopt -I
OCAMLDEP  = $(CAML)dep

# CAMLIMAGESDIR & CAMLIMAGESLIBS is defined in Makefile.config

MLINCDIRS = $(CAMLIMAGESDIR) $(LABLGTKDIR)

EXEC	  = advi

MISCMODULES = config misc options rc userfile
TEXMODULES = input symbol search table pkfont ttfont jfm font glyph \
		units dimension dvi devfont
GUIMODULES = ageometry dvicolor
GTKMODULES = grMisc grDrawable grDbuffer grCursor grSubwindow \
		grImage grGlyph grSleep grDvi grDev
TEXGUIMODULES = driver dviview main
MODULES = $(MISCMODULES) $(TEXMODULES) $(GUIMODULES) $(GTKMODULES) $(TEXGUIMODULES)

# 	      scratch launch \
# 	      shot drawimage \
# 	      gs transimpl embed ageometry thumbnails \
# 	      dviview main

LIBRARIES = unix str $(LABLGTKLIBS) $(CAMLIMAGESLIBS) 
CLIBS	  = unix str

COBJS     = 

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

all: $(INSTALLTARGET) documentation
allopt: opt
opt: $(EXEC).opt 
byte: $(EXEC)

$(EXEC): $(COBJS) $(CMO_OBJS)
	$(OCAMLC) -custom $(INCLUDES) $(BYTE_OBJS) $(LINK_OPTS) -o $(EXEC)

$(EXEC).opt: $(COBJS) $(CMX_OBJS)
	$(OCAMLOPT) $(INCLUDES) $(OPT_OBJS) $(LINK_OPTS) -o $(EXEC).opt

config.ml: config.ml.in configure
	./configure

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
	rm -f *.cm[oix] *.o $(EXEC) $(EXEC).opt *~ *.log *.aux */*.cm[oix]
	cd test; $(MAKE) clean
	cd doc; $(MAKE) clean

veryclean: clean
	rm -f Makefile.config config.cache config.log \
	config.status drawps.ml ttfont.ml config.ml ifdef.ml .depend

veryveryclean: veryclean
	rm -f configure

installopt: install
install:: $(INSTALLTARGET) doc/splash.dvi
	cp $(INSTALLTARGET) ${bindir}/advi
	- mkdir -p $(ADVI_LOC)
	cp doc/splash.dvi tex/advilogo.eps tex/caml.eps tex/bar.jpg.eps tex/*.sty tex/advi.pro $(ADVI_LOC)
	if [ -f conf/jpfonts.conf ]; then cp conf/jpfonts.conf $(ADVI_LOC); fi

MLFILES = $(addsuffix .ml, $(MODULES))

.depend:: *.mli $(MLFILES) Makefile
	$(OCAMLDEP) *.mli */*.mli $(MLFILES) > .depend

# just for the authors
ADVI=advi-$(VERSION)
WEBSITEDIR=/net/pauillac/infosystems/www/advi
FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal/advi

# make splash builds splash.dvi
doc/splash.dvi:
	(cd doc; $(MAKE) splash.dvi)

documentation:
	(cd doc; $(MAKE) all)

distribute: tar_and_web rpm

tar_and_web: tex/splash.dvi
	(cd test; $(MAKE) all jpdemo.dvi)
	rm -rf release
	rm -rf $(WEBSITEDIR)/*
	- mkdir $(WEBSITEDIR)
	mkdir release
	cd release; cvs co bazar-ocaml/advi; \
	cp -p ../test/*.dvi bazar-ocaml/advi/test/; \
	cp -p ../doc/manual.dvi bazar-ocaml/advi/doc/; \
	cp -p ../doc/manual.ps bazar-ocaml/advi/doc/; \
	cp -p ../doc/manual.pdf bazar-ocaml/advi/doc/; \
	cp -p ../doc/splash.dvi bazar-ocaml/advi/doc/; \
	cp -p ../tex/advi.pro bazar-ocaml/advi/test/; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	find . -name 'advi-development-kit' -print | xargs rm -rf; \
	(cd bazar-ocaml/advi/doc/; \
	ln -s index.html index-en.html; cd ../../..); \
	cp -pr bazar-ocaml/advi/doc/* $(WEBSITEDIR)
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)
	cp -pr release/bazar-ocaml/advi/LGPL release/bazar-ocaml/advi/README release/bazar-ocaml/advi/INDEX $(FTPSITEDIR)
	cd release; mv bazar-ocaml/advi $(ADVI); \
	tar cvf $(ADVI).tar $(ADVI); \
	gzip $(ADVI).tar; \
	mv -f $(ADVI).tar.gz $(FTPSITEDIR)
	rm -rf release
	cd advi-development-kit; $(MAKE) distribute

web: tex/splash.dvi
	(cd test; $(MAKE) all jpdemo.dvi)
	rm -rf release
	rm -rf $(WEBSITEDIR)/*
	- mkdir $(WEBSITEDIR)
	mkdir release
	cd release; cvs co bazar-ocaml/advi; \
	cp -p ../test/*.dvi bazar-ocaml/advi/test/; \
	cp -p ../doc/manual.dvi bazar-ocaml/advi/doc/; \
	cp -p ../doc/manual.ps bazar-ocaml/advi/doc/; \
	cp -p ../doc/manual.pdf bazar-ocaml/advi/doc/; \
	cp -p ../doc/splash.dvi bazar-ocaml/advi/doc/; \
	cp -p ../tex/advi.pro bazar-ocaml/advi/test/; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	find . -name 'advi-development-kit' -print | xargs rm -rf; \
	(cd bazar-ocaml/advi/doc/; \
	ln -s index.html index-en.html; cd ../../..); \
	cp -pr bazar-ocaml/advi/doc/* $(WEBSITEDIR)
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)
	rm -rf release


rpm:
	if test -d /usr/src/redhat; then rpmdir=/usr/src/redhat; \
	else if test -d /usr/src/RPM; then rpmdir=/usr/src/RPM; fi; fi; \
	if test "X$$rpmdir" = "X"; then \
		echo "cannot create rpm"; exit 2; fi; \
	echo YOU NEED TO SU ROOT; \
	su root -c "cp $(FTPSITEDIR)/$(ADVI).tar.gz $$rpmdir/SOURCES/; rpm -ba --clean ./advi.spec"; \
	cp $$rpmdir/SRPMS/advi-$(VERSION)-1.src.rpm \
	   $$rpmdir/RPMS/*/advi-$(VERSION)-1.*.rpm $(FTPSITEDIR)

include .depend
