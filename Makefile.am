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
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

bin_PROGRAMS = advi.byt advi.opt
OCAMLBUILTSOURCES = config.ml
OCAMLSOURCES = misc.ml timeout.ml ageometry.ml \
	       options.ml rc.ml userfile.ml \
	       graphicsY11.ml global_options.ml busy.ml gradient.ml gterm.ml \
	       launch.ml dvicolor.ml shot.ml laser_pointer.ml \
	       symbol.ml \
	       input.ml table.ml pkfont.ml ttfont.ml jfm.ml search.ml \
	       font.ml glyph.ml devfont.ml units.ml dimension.ml dvi.ml \
	       drawimage.ml gs.ml transimpl.ml embed.ml grdev.ml addons.ml \
	       scratch.ml cdvi.ml driver.ml thumbnails.ml dviview.ml main.ml
CSOURCES = events.c grwm.c grY11.c
advi_opt_SOURCES = $(OCAMLSOURCES) $(CSOURCES)
advi_byt_SOURCES = $(OCAMLSOURCES) $(CSOURCES)

BUILT_SOURCES = $(OCAMLBUILTSOURCES)

EXTRA_DIST = config.ml.in

CLEANFILES = $(OCAMLBYTOBJS) \
             $(OCAMLOPTOBJS) \
             $(OCAMLOPTOBJS:.cmx=.o) \
             $(COBJS) \
	     config.ml \
	     .depend

OCAMLBYTOBJS    = $(OCAMLBUILTSOURCES:.ml=.cmo) $(OCAMLSOURCES:.ml=.cmo)
OCAMLOPTOBJS    = $(OCAMLBUILTSOURCES:.ml=.cmx) $(OCAMLSOURCES:.ml=.cmx)
COBJS           = $(CSOURCES:.c=.o)

MLLIBS = graphics unix str $(CAMLIMAGESLIBS)
CLIBS  = graphics unix str

CMA_OBJS  = $(addsuffix .cma, $(MLLIBS))
CMXA_OBJS = $(addsuffix .cmxa, $(MLLIBS))

AM_CFLAGS	= $(XINERAMA_CFLAGS) $(X_CFLAGS)
AM_LIBS		= $(X_LIBS) $(X_PRE_LIBS) $(XINERAMA_LIBS) -lX11 $(X_EXTRA_LIBS)

AM_COMPFLAGS	= $(CAMLIMAGES_INCLUDES) $(LABLTK_INCLUDES)
AM_LINKFLAGS	= $(addprefix -ccopt -L, $(CLIBDIRS)) \
		  $(addprefix -cclib -l, $(CLIBS)) \
		  $(addprefix -cclib , $(AM_LIBS))

OTHER_LIBS	= unix str
OCAMLLIBS_OPT	= $(CAMLIMAGES_LIBS_NATIVE) $(OTHER_LIBS:=.cmxa)
OCAMLLIBS_BYT	= $(CAMLIMAGES_LIBS_BYTE) $(OTHER_LIBS:=.cma)

all: byt opt

byt: advi.byt$(EXEEXT)

opt: advi.opt$(EXEC)

advi.byt$(EXEEXT): $(COBJS) $(OCAMLBYTOBJS)
	$(OCAMLC) -o advi.byt$(EXEEXT) \
		$(COMPFLAGS) $(AM_COMPFLAGS) $(AM_LINKFLAGS) -custom \
		$(OCAMLLIBS_BYT) $(OCAMLBYTOBJS)

advi.opt$(EXEEXT): $(COBJS) $(OCAMLOPTOBJS)
	$(OCAMLOPT) -o advi.opt$(EXEEXT) \
		$(COMPFLAGS) $(AM_COMPFLAGS) $(AM_LINKFLAGS) \
		$(COBJS) $(OCAMLLIBS_OPT) $(OCAMLOPTOBJS)

install_hook: $(INSTALLEXEC) installdata
	- texhash
	@ if test -z "`kpsewhich advi.sty`"; then \
	  echo '*** NOTE BEFORE USE ***' ;\
	  echo Please add $(ADVI_LOC); \
	  echo to your TEXINPUTS environment variable\!; \
	  echo '***********************' ;\
	fi

.depend: $(OCAMLSOURCES) 
	$(OCAMLDEP) $(OCAMLSOURCES) $(OCAMLINTERFACES) > .depend

config.ml: config.ml.in Makefile
	rm -f $@ $@.tmp
	sed \
		-e 's,@VERSION\@,$(VERSION),g' \
		-e 's,@BUILD_DATE\@,$(BUILD_DATE),g' \
		-e 's,@HAVE_GS\@,$(HAVE_GS),g' \
		-e 's,@TEXMFMAIN\@,$(TEXMFMAIN),g' \
		-e 's,@ADVI_LOC\@,$(ADVI_LOC),g' \
		$< > $@.tmp
	mv $@.tmp $@


SUFFIXES = .ml .mli .cmo .cmi .cmx .c .o

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) $(AM_COMPFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) $(AM_COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) $(AM_COMPFLAGS) -c $<

.c.o:
	$(OCAMLC) -ccopt "$(CFLAGS) $(AM_CFLAGS)" -c $<

# ugly stuff for make time include
@AMDEP_TRUE@@am__include@ @am__quote@.depend@am__quote@
