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

# $Id$

include Makefile.config

YEAR=2004
OLDYEAR=2003
PACKAGE=advi
MAINVERSION=1
SUBVERSION=6
PATCHLEVEL=0
VERSION=$(MAINVERSION).$(SUBVERSION)
OLDVERSION=1.5

CVSRELEASETAG=$(PACKAGE)-
ANNOUNCEFILE=Announce-$(VERSION).$(PATCHLEVEL)

PACKAGEVERSIONFILE=config.ml
DOCVERSIONFILES=tex/advi.sty tex/advi.hva \
doc_src/Includes/advi-version.html doc_src/Includes/env \
doc/advi.1

HELPFILES=doc/splash.dvi \
    doc/scratch_write_splash.dvi doc/scratch_draw_splash.dvi

TEXSTYFILES= \
    advi-annot.sty advi-slides.sty argv.sty superpose.sty \
    advi-graphicx.sty advi.sty bubble.sty xwindows-colors.sty

TEXEPSFILES= \
    advilogo.eps bar.eps caml.eps

STYFILES= $(addprefix tex/, $(TEXSTYFILES))
EPSFILES= $(addprefix tex/, $(TEXEPSFILES))

COPTIONS = -warn-error A -g
COPTOPTIONS = -warn-error A -inline 9

OCAMLC	  = $(CAML)c $(COPTIONS)
OCAMLOPT  = $(CAML)opt.opt $(COPTOPTIONS)
# OCAMLOPT  = $(CAML)opt.opt -unsafe -inline 9 -ccopt -I
OCAMLDEP  = $(CAML)dep

# CAMLIMAGESDIR & CAMLIMAGESLIBS is defined in Makefile.config

MLINCDIRS = $(CAMLIMAGESDIR)

EXEC	  = advi

MISC	  = config misc timeout ageometry 
OPTIONS	  = options rc userfile
GRAPHICS  = graphicsY11 global_options busy gradient gterm launch \
	    dvicolor shot laser_pointer
SYMBOL	  = symbol
DVI	  = input table pkfont ttfont jfm search \
	    font glyph devfont units dimension dvi
EFFECTS	  = drawimage gs transimpl embed
GUI	  = scratch cdvi driver thumbnails dviview

MODULES	  = $(MISC) $(OPTIONS) $(GRAPHICS) \
            $(SYMBOL) $(DVI) \
            $(EFFECTS) grdev addons \
	    $(GUI) main

LIBRARIES = graphics unix str $(CAMLIMAGESLIBS)
CLIBS	  = graphics unix str

COBJS     = events.o grwm.o grY11.o

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
	    $(addprefix -cclib , $(WITH_X))

X11_INCLUDES=-I/usr/X11R6/include
BYTECCCOMPOPTS=-fno-defer-pop -Wall -Wno-unused
CFLAGS= $(EXTRA_X11) $(X11_INCLUDES) -O $(BYTECCCOMPOPTS)

default: Makefile.config $(INSTALLTARGET) $(HELPFILES)

all: $(INSTALLTARGET) documentation
allopt: opt documentation
allbyte: byte documentation
opt: $(EXEC).opt 
byte: $(EXEC)

i_want_opt:
	@echo "************************** Warning ***************************"
	@echo "  You have no native code version of the O'Caml compiler."
	@echo "  For the best exprerience, we strongly recommend to use "
	@echo "  Active-DVI compiled with the native code compiler (ocamlopt)."
	@echo "  For further information about ocamlopt, look at:"
	@echo "    http://www.caml.org/"
	@echo "  If there is no ocamlopt compiler version for your platform,"
	@echo "  you can still get a slow but fully functional version of"
	@echo "  Active-DVI, by typing \"make allbyte\" that would build a byte"
	@echo "  code version (\"make installbyte\" for install.)"
	@echo "**************************************************************"
	@exit 1        

$(EXEC): $(COBJS) $(CMO_OBJS)
	$(OCAMLC) -custom $(INCLUDES) $(BYTE_OBJS) $(LINK_OPTS) -o $(EXEC)

$(EXEC).opt: $(COBJS) $(CMX_OBJS)
	$(OCAMLOPT) $(INCLUDES) $(OPT_OBJS) $(LINK_OPTS) -o $(EXEC).opt

config.ml: config.ml.in configure
	./configure

version:
	for i in $(PACKAGEVERSIONFILE) $(DOCVERSIONFILES); do \
	echo $$i; \
	mv $$i $$i~; \
	sed -e '/ersion/s/$(OLDVERSION)/$(VERSION)/' $$i~ > $$i~~; \
	sed -e '/year/s/$(OLDYEAR)/$(YEAR)/' $$i~~ > $$i; \
	rm $$i~~; \
	done

.c.o:
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
	cd test && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd examples && $(MAKE) clean

veryclean: clean
	rm -f Makefile.config config.cache config.log \
	config.status config.ml

veryveryclean: veryclean
	rm -f configure

installopt: install
installbyte: 
	$(MAKE) install INSTALLTARGET=advi
install:: $(INSTALLTARGET) $(HELPFILES)
	- install -d ${bindir}
	install -m 755 $(INSTALLTARGET) ${bindir}/advi
	- install -d $(ADVI_LOC)
	install -m 644 $(HELPFILES) $(EPSFILES)	$(STYFILES) $(ADVI_LOC)
	if [ -f conf/jpfonts.conf ]; then \
		install -m 644 conf/jpfonts.conf $(ADVI_LOC); fi
	texhash
	@ if test "x`kpsewhich advi.sty`" = "x"; then \
	  echo '*** NOTE BEFORE USE ***' ;\
	  echo Please add $(ADVI_LOC); \
	  echo to your TEXINPUTS environment variable\!; \
	  echo '***********************' ;\
	fi

MLFILES = $(addsuffix .ml, $(MODULES))

Makefile.config: Makefile.config.in
	./configure_FreeBSD

.depend:: Makefile
	$(OCAMLDEP) *.mli $(MLFILES) > .depend
	gcc -MM -I$(CAMLDIR) $(CFLAGS) $(COBJS:.o=.c) \
		| sed -e 's|$(CAMLDIR)/[^ ]*||' >> .depend
	chmod a+w .depend

# Just for the authors
ADVI=advi-$(VERSION)
WEBSITEDIR=/net/pauillac/infosystems/www/advi
FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal/advi

# make splash builds splash.dvi
doc/splash.dvi:
	(cd doc; $(MAKE) splash.dvi)

doc/scratch_draw_splash.dvi:
	(cd doc; $(MAKE) scratch_draw_splash.dvi)

doc/scratch_write_splash.dvi:
	(cd doc; $(MAKE) scratch_write_splash.dvi)

documentation:
	if test $(HAVE_HEVEA) = "true"; then (cd doc; $(MAKE) all); fi

distribute: tar_and_web

tar_and_web:
	$(MAKE) web_site;
	cp -pr release/bazar-ocaml/advi/LGPL \
		release/bazar-ocaml/advi/README \
		release/bazar-ocaml/advi/INDEX $(FTPSITEDIR)
	cd release; mv bazar-ocaml/advi $(ADVI); \
	tar cvf $(ADVI).tar $(ADVI); \
	gzip $(ADVI).tar; \
	mv -f $(ADVI).tar.gz $(FTPSITEDIR)
	$(MAKE) clean_release
	cd advi-development-kit; $(MAKE) distribute

web:
	$(MAKE) web_site
	$(MAKE) clean_release

web_site:
	$(MAKE) clean_release
	(cd test; $(MAKE) all jpdemo.dvi)
	(cd doc; $(MAKE) distribute)
	rm -rf $(WEBSITEDIR)/*
	- mkdir $(WEBSITEDIR)
	mkdir release
	cd release; cvs co bazar-ocaml/advi; \
	cp -p ../test/*.dvi bazar-ocaml/advi/test/; \
	cp -p ../doc/*.dvi bazar-ocaml/advi/doc/; \
	cp -p ../doc/*.ps bazar-ocaml/advi/doc/; \
	cp -p ../doc/*.pdf bazar-ocaml/advi/doc/; \
	cp -p ../doc/*.html bazar-ocaml/advi/doc/; \
	cp -p ../doc/*.htm bazar-ocaml/advi/doc/; \
	cp -p ../doc/*.gif bazar-ocaml/advi/doc/; \
	cp -p ../doc/*.jpg bazar-ocaml/advi/doc/; \
	cp -p ../doc/style.css bazar-ocaml/advi/doc/; \
	cp -p -r ../doc/pngs bazar-ocaml/advi/doc/; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	find . -name 'advi-development-kit' -print | xargs rm -rf; \
	(cd bazar-ocaml/advi/doc/; \
	ln -sf eng.html index.html; cd ../../..); \
	cp -pr bazar-ocaml/advi/doc/* $(WEBSITEDIR)
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)

clean_release:
	rm -rf release

release:
	cvs rtag -R $(CVSRELEASETAG) bazar-ocaml/$(PACKAGE)

announce:
	mail -n -s "New release of $(PACKAGE)" \
		caml-announce@inria.fr < $(ANNOUNCEFILE)

rpm:
	if test -d /usr/src/redhat; then rpmdir=/usr/src/redhat; \
	else if test -d /usr/src/RPM; then rpmdir=/usr/src/RPM; \
	else if test -d /usr/src/rpm; then rpmdir=/usr/src/rpm; fi fi; fi; \
	if test "X$$rpmdir" = "X"; then \
		echo "cannot create rpm"; exit 2; fi; \
	echo YOU NEED TO SU ROOT; \
	su root -c "cp $(FTPSITEDIR)/$(ADVI).tar.gz $$rpmdir/SOURCES/; \
	rpm -ba --clean ./advi.spec"; \
	cp $$rpmdir/SRPMS/advi-$(VERSION)-1.src.rpm \
	   $$rpmdir/RPMS/*/advi-$(VERSION)-1.*.rpm $(FTPSITEDIR)

include .depend
