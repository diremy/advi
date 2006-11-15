#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2002-2004,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  All rights reserved. This file is distributed under the terms of   #
#  the GNU Lesser General Public License.                             #
#                                                                     #
#  Jun Furuse, Didier R�my and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

PACKAGE_VERSION	= 1.7.0
PACKAGE_DATE	= 2006-11-13
PATH_GS		= /usr/bin/gs
PATH_KPSEWHICH	= /usr/bin/kpsewhich
PATH_GZIP	= /bin/gzip
TEXMFMAIN	= /usr/share/texmf

RM=rm -fR
MV=mv -f
CP=cp -pfR

MANEXT=1
MANDIR=${prefix}/man
CAML=ocaml
CAMLDIR=/usr/lib/ocaml
CAMLVERSION=3.08.4
INSTALLTARGET=advi.bin

WITH_X= -L/usr/X11R6/lib  -lSM -lICE -lXinerama -lXext -lX11 

HAVE_CAMLIMAGES=true
include /usr/lib/ocaml/camlimages/Makefile.config
CAMLIMAGESLIBS=$(WITH_CAMLIMAGES:.cma=)
CAMLIMAGESDIR=/usr/lib/ocaml/camlimages

HAVE_LABLTK=true
LABLTKDIR=/usr/lib/ocaml/labltk
LABLTKLIB=/usr/lib/ocaml/labltk/labltk.cma

HAVE_CDK=@HAVE_CDK@

HAVE_GS=true
EXTRA_X11=-DHAVE_XINERAMA

srcdir=.
top_srcdir=.

prefix=/usr/local
exec_prefix=${prefix}

bindir=${exec_prefix}/bin
sbindir=${exec_prefix}/sbin
libexecdir=${exec_prefix}/libexec
datadir=${prefix}/share
sysconfdir=${prefix}/etc
sharedstatedir=${prefix}/com
localstatedir=${prefix}/var
libdir=${exec_prefix}/lib
infodir=${prefix}/info
mandir=${prefix}/man
includedir=${prefix}/include

ADVI_LOC=/usr/share/texmf/tex/latex/advi

HAVE_HEVEA=true
PATH_HEVEA=/home/remy/bin/hevea

YEAR = 2005
OLDYEAR = 2004
PACKAGE = advi
MAINVERSION = 1
SUBVERSION = 7
PATCHLEVEL = 0
VERSION = $(MAINVERSION).$(SUBVERSION)
FULLVERSION = $(VERSION).$(PATCHLEVEL)
OLDVERSION = 1.6

PACKAGEFULLNAME = $(PACKAGE)-$(FULLVERSION)

CVSRELEASETAG = $(PACKAGE)-$(MAINVERSION)_$(SUBVERSION)_$(PATCHLEVEL)
ANNOUNCEFILE = Announce-$(FULLVERSION)

PACKAGEVERSIONFILES = config.ml
DOCVERSIONFILES = tex/advi.sty tex/advi.hva \
doc_src/Includes/advi-version.html doc_src/Includes/env \
doc_src/advi.man

MANFILES = doc/advi.1

HELPFILES = doc/splash.dvi \
    doc/scratch_write_splash.dvi doc/scratch_draw_splash.dvi

TEXSTYFILES = \
    advi-annot.sty advi-slides.sty argv.sty superpose.sty \
    advi-graphicx.sty advi.sty bubble.sty xwindows-colors.sty

TEXEPSFILES = \
    advilogo.eps bar.eps caml.eps

STYFILES = $(addprefix tex/, $(TEXSTYFILES))
EPSFILES = $(addprefix tex/, $(TEXEPSFILES))

BYTOPTIONS = -warn-error A -g
BINOPTIONS = -warn-error A -inline 10000

CAMLBYT = $(CAML)c $(BYTOPTIONS)
CAMLBIN = $(CAML)opt.opt $(BINOPTIONS)
CAMLDEP = $(CAML)dep

MLINCDIRS = $(CAMLIMAGESDIR)

EXEC	 = advi

MISC	 = config misc timeout ageometry
OPTIONS	 = options rc userfile
GRAPHICS = graphicsY11 global_options busy gradient gterm launch \
	   dvicolor shot laser_pointer
SYMBOL	 = symbol
DVI	 = input table pkfont ttfont jfm search \
	   font glyph devfont units dimension dvi
EFFECTS	 = drawimage gs transimpl embed
GUI	 = scratch cdvi driver thumbnails dviview

MODULES	 = $(MISC) $(OPTIONS) $(GRAPHICS) \
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

BYT_OBJS = $(COBJS) $(CMA_OBJS) $(CMO_OBJS)
BIN_OBJS = $(COBJS) $(CMXA_OBJS) $(CMX_OBJS)

CAMLINCLUDES  = $(addprefix -I , $(MLINCDIRS))
LINK_OPTS = $(addprefix -ccopt -L, $(CLIBDIRS)) \
	    $(addprefix -cclib -l, $(CLIBS)) \
	    $(addprefix -cclib , $(WITH_X))

X11_INCLUDES=-I/usr/X11R6/include
BYTCCCOMPOPTS=-fno-defer-pop -Wall -Wno-unused
CFLAGS=$(EXTRA_X11) $(X11_INCLUDES) -O $(BYTCCCOMPOPTS)

edit = sed \
    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
    -e 's,@PACKAGE_DATE\@,$(PACKAGE_DATE),g' \
    -e 's,@HAVE_GS\@,$(HAVE_GS),g' \
    -e 's,@PATH_KPSEWHICH\@,$(PATH_KPSEWHICH),g' \
    -e 's,@PATH_GZIP\@,$(PATH_GZIP),g' \
    -e 's,@PATH_GS\@,$(PATH_GS),g' \
    -e 's,@TEXMFMAIN\@,$(TEXMFMAIN),g' \
    -e 's,@ADVI_LOC\@,$(ADVI_LOC),g'

all: byt bin doc

i_want_opt:
	@echo "************************** Warning ***************************"
	@echo "  You have no native code version of the O'Caml compiler."
	@echo "  For the best exprerience, we strongly recommend to use "
	@echo "  Active-DVI compiled with the native code compiler (ocamlopt)."
	@echo "  For further information about ocamlopt, look at:"
	@echo "    http://caml.inria.fr/"
	@echo "  If there is no ocamlopt compiler version for your platform,"
	@echo "  you can still get a slow but fully functional version of"
	@echo "  Active-DVI, by typing \"make allbyt\" that would build a byte"
	@echo "  code version (\"make installbyt\" for install.)"
	@echo "**************************************************************"
	@exit 1

byt: $(EXEC).byt

$(EXEC).byt: $(COBJS) $(CMO_OBJS)
	$(CAMLBYT) -custom $(CAMLINCLUDES) $(BYT_OBJS) \
		$(LINK_OPTS) -o $(EXEC).byt

bin: $(EXEC).bin

$(EXEC).bin: $(COBJS) $(CMX_OBJS)
	$(CAMLBIN) $(CAMLINCLUDES) $(BIN_OBJS) \
		$(LINK_OPTS) -o $(EXEC).bin

documentation: $(MANFILES)
	cd doc; $(MAKE) all

doc: $(HELPFILES)
	if test $(HAVE_HEVEA) = "true"; then ($(MAKE) documentation); fi

veryclean: clean
	rm -f config.cache config.log \
	config.status config.ml

veryveryclean: veryclean
	rm -f configure

install: installbin installman

installbyt:
	$(MAKE) install INSTALLTARGET=advi.byt

installbin:: $(INSTALLTARGET) $(HELPFILES)
	- install -d ${bindir}
	install -m 755 $(INSTALLTARGET) ${bindir}/advi
	- install -d $(ADVI_LOC)
	install -m 644 $(HELPFILES) $(EPSFILES)	$(STYFILES) $(ADVI_LOC)
	- install -d $(MANDIR)/man$(MANEXT)
	if [ -f conf/jpfonts.conf ]; then \
		install -m 644 conf/jpfonts.conf $(ADVI_LOC); fi
	texhash
	@ if test "x`kpsewhich advi.sty`" = "x"; then \
	  echo '*** NOTE BEFORE USE ***' ;\
	  echo Please add $(ADVI_LOC); \
	  echo to your TEXINPUTS environment variable\!; \
	  echo '***********************' ;\
	fi

installman:
	install -m 644 $(MANFILES) $(MANDIR)/man$(MANEXT) || exit 0

MLFILES = $(addsuffix .ml, $(MODULES))

clean::
	$(RM) $(EXEC).bin $(EXEC).byt
	cd test && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd examples && $(MAKE) clean

.depend:: Makefile
	$(CAMLDEP) *.mli $(MLFILES) > .depend
	gcc -MM -I$(CAMLDIR) $(CFLAGS) $(COBJS:.o=.c) \
		| sed -e 's|$(CAMLDIR)/[^ ]*||' >> .depend
	chmod a+w .depend

doc/splash.dvi: doc/splash.tex
	cd doc; $(MAKE) `basename $@`
doc/scratch_write_splash.dvi: doc/scratch_write_splash.tex
	cd doc; $(MAKE) `basename $@`
doc/scratch_draw_splash.dvi: doc/scratch_draw_splash.tex
	cd doc; $(MAKE) `basename $@`

doc/advi.1: doc_src/advi.man
	cd doc; $(MAKE) `basename $@`

# Just for the authors

# Automatic handling of versionning
version:
	for i in $(PACKAGEVERSIONFILES) $(DOCVERSIONFILES); do \
	echo $$i; \
	$(MV) $$i $$i~; \
	sed -e '/ersion/s/$(OLDVERSION)/$(VERSION)/' $$i~ | \
	sed -e '/year/s/$(OLDYEAR)/$(YEAR)/' > $$i; \
	done

distribution: all documentation
	$(MAKE) -f Makefile.distrib distribute

release:
	cvs rtag -R $(CVSRELEASETAG) bazar-ocaml/$(PACKAGE)

unrelease:
	$(RM) ./release
	cvs rtag -R -d $(CVSRELEASETAG) bazar-ocaml/$(PACKAGE)

announce:
	mail -n -s "New release of $(PACKAGE)" \
		caml-announce@inria.fr < $(ANNOUNCEFILE)

package_distribution: release distribution announce	

count:
	wc -l *.ml *.mli | sort -n

config.ml: config.ml.in Makefile
	rm -f $@ $@.tmp
	$(edit) $< > $@.tmp
	mv $@.tmp $@

include Makefile.common
include .depend
