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
include Makefile.config

MISC	 = config misc timeout ageometry
OPTIONS	 = options rc userfile
GRAPHICS = graphicsY11 global_options busy gradient gterm launch \
	   dvicolor shot laser_pointer
SYMBOL	 = symbol
DVI	 = input table pkfont ttfont jfm search \
	   font glyph devfont units dimension dvi
EFFECTS	 = drawimage gs transimpl embed grdev addons
GUI	 = scratch cdvi driver thumbnails dviview main

MLMODULES	= $(MISC) $(OPTIONS) $(GRAPHICS) \
		  $(SYMBOL) $(DVI) \
		  $(EFFECTS) \
		  $(GUI)

CMODULES  = events grwm grY11

MLFILES = $(addsuffix .ml, $(MLMODULES))

LIBRARIES = graphics unix str $(CAMLIMAGESLIBS)
CLIBS	  = graphics unix str

COBJS     = $(addsuffix .o, $(CMODULES))

CMO_OBJS  = $(addsuffix .cmo, $(MLMODULES))
CMX_OBJS  = $(addsuffix .cmx, $(MLMODULES))

CMA_OBJS  = $(addsuffix .cma, $(LIBRARIES))
CMXA_OBJS = $(addsuffix .cmxa, $(LIBRARIES))

BYT_OBJS = $(COBJS) $(CMA_OBJS) $(CMO_OBJS)
BIN_OBJS = $(COBJS) $(CMXA_OBJS) $(CMX_OBJS)

BYTCCCOMPOPTS	= -fno-defer-pop -Wall -Wno-unused
LOCAL_CFLAGS	= $(XINERAMA_CFLAGS) $(X_CFLAGS) -O $(BYTCCCOMPOPTS)
LOCAL_LIBS	= $(X_LIBS) $(X_PRE_LIBS) $(XINERAMA_LIBS) -lX11 $(X_EXTRA_LIBS)

CAMLINCLUDES  = $(addprefix -I , $(CAMLIMAGESDIR))
LOCAL_LINKFLAGS	= $(addprefix -ccopt -L, $(CLIBDIRS)) \
		  $(addprefix -cclib -l, $(CLIBS)) \
		  $(addprefix -cclib , $(LOCAL_LIBS))


all: byt bin doc

byt: $(EXEC).byt

$(EXEC).byt: $(COBJS) $(CMO_OBJS)
	$(OCAMLC) $(BYT_COMPFLAGS) -custom $(CAMLINCLUDES) $(BYT_OBJS) \
		$(LOCAL_LINKFLAGS) -o $(EXEC).byt

bin: $(EXEC).bin

$(EXEC).bin: $(COBJS) $(CMX_OBJS)
	$(OCAMLOPT) $(BIN_COMPFLAGS) $(CAMLINCLUDES) $(BIN_OBJS) \
		$(LOCAL_LINKFLAGS) -o $(EXEC).bin

doc:
	cd doc; $(MAKE) all

veryclean: clean
	rm -f config.cache config.log \
	config.status config.ml

veryveryclean: veryclean
	rm -f configure

install: $(INSTALLEXEC) installdata
	- texhash
	@ if test -z "`kpsewhich advi.sty`"; then \
	  echo '*** NOTE BEFORE USE ***' ;\
	  echo Please add $(ADVI_LOC); \
	  echo to your TEXINPUTS environment variable\!; \
	  echo '***********************' ;\
	fi

installdata:
	- $(INSTALL) -d $(DESTDIR)$(ADVI_LOC)
	cd doc; $(MAKE) install
	cd tex; $(MAKE) install
	cd conf; $(MAKE) install

installbyt: $(EXEC).byte
	- $(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -m 755 $(EXEC).byte $(DESTDIR)$(bindir)/$(EXEC)

installbin: $(EXEC).bin
	- $(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL) -m 755 $(EXEC).bin $(DESTDIR)$(bindir)/$(EXEC)

clean:
	$(RM) $(EXEC).bin $(EXEC).byt
	$(RM) *.cm[oix] *.o *~ .depend *.log *.aux
	cd test && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd examples && $(MAKE) clean

.depend:: Makefile
	$(OCAMLDEP) *.mli $(MLFILES) > .depend
	$(CC) -MM -I$(OCAMLLIB) $(LOCAL_CFLAGS) $(COBJS:.o=.c) \
		| sed -e 's|$(OCAMLLIB)/[^ ]*||' >> .depend
	chmod a+w .depend

config.ml: config.ml.in Makefile.config
	rm -f $@ $@.tmp
	sed \
		-e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
		-e 's,@BUILD_DATE\@,$(BUILD_DATE),g' \
		-e 's,@HAVE_GS\@,$(HAVE_GS),g' \
		-e 's,@TEXMFMAIN\@,$(TEXMFMAIN),g' \
		-e 's,@ADVI_LOC\@,$(ADVI_LOC),g' \
		$< > $@.tmp
	mv $@.tmp $@

.c.o:
	$(OCAMLC) -ccopt "$(LOCAL_CFLAGS)" -c $<

.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o

.mli.cmi:
	$(OCAMLC) $(BYT_COMPFLAGS) $(CAMLINCLUDES) -c $<

.ml.cmo:
	$(OCAMLC) $(BYT_COMPFLAGS) $(CAMLINCLUDES) -c $<

.ml.cmx:
	$(OCAMLOPT) $(BIN_COMPFLAGS) $(CAMLINCLUDES) -c $<

include .depend
