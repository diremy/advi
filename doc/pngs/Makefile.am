#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2003 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

pngdir = $(datadir)/doc/$(PACKAGE)/pngs
nobase_png_DATA = $(STATICIMAGES) $(BUILTIMAGES)

STATICIMAGES = basics/adviwait.png \
	       basics/moderate.png \
	       basics/simple.png \
	       basics/cash/cash.png \
	       japanese/advi-j.png \
	       prosper/Join/behaviour_pi.png \
	       prosper/Join/join.png \
	       prosper/Join/join_title.png \
	       prosper/Join/pi.png \
	       prosper/LL/LL_def.png \
	       prosper/LL/LL_launch.png \
	       prosper/LL/LL_libre_vs_proprietaire.png \
	       prosper/LL/LL_title.png \
	       seminar/a14/a14_bezier.png \
	       seminar/a14/a14_fill.png \
	       seminar/a14/a14_title.png \
	       seminar/a14/bitmap.png \
	       seminar/clock/OCamlODBC.png \
	       seminar/clock/clock_title.png \
	       slitex/advi/advi.png \
	       slitex/advi/advi_end.png \
	       slitex/advi/interaction.png \
	       slitex/advi/interaction_source.png \
	       slitex/advi/test_colors.png \
	       slitex/caml/caml.png \
	       slitex/caml/caml_future.png \
	       slitex/caml/caml_strength.png \
	       slitex/caml/code_evolutivity.png \
	       slitex/simplistic/simplistic.png \
	       slitex/simplistic/simplistic_1.png \
	       slitex/simplistic/simplistic_plan.png \
	       slitex/simplistic/simplistic_source.png \
	       slitex/simplistic/simplistic_title.png \
	       slitex/skin_detection/skin_detection.png \
	       slitex/skin_detection/skin_detection_title.png \
	       slitex/skin_detection/skin_illumination.png \
	       slitex/skin_detection/skin_variation.png \
	       test/alpha_channel.png \
	       test/annotations.png \
	       test/caml_whale.png \
	       test/camltk_embedding.png \
	       test/demo2.png \
	       test/mldvi.png \
	       test/test_colors.png \
	       test/text_movements.png \
	       test/yellow_background.png

CLEANFILES = $(STATICIMAGES:.png=.tiny.png)

MAINTAINERCLEANFILES = f.png \
		       bigf.png uf.png cf.png lf.png \
		       $(STATICIMAGES:.png=.small.png)

EXTRA_DIST = $(nobase_png_DATA)

SMALL_HEIGHT = 170
SMALL_WIDTH = 230
TINY_HEIGHT = 85
TINY_WIDTH = 115
FRISE_HEIGHT = 85
FRISE_WIDTH = 1600

f.png: bigf.png
	convert -geometry 800x150! bigf.png f.png

bigf.png: uf.png cf.png lf.png
	montage -tile 1x3 -geometry $(FRISE_WIDTH)x$(FRISE_HEIGHT)! \
		uf.png cf.png lf.png bigf.png

uf.png: $(STATICIMAGES:.png=.tiny.png)
	montage -tile $(FRISE_WIDTH)x$(FRISE_HEIGHT) \
		-mode Concatenate -background white -adjoin \
		`find . -regex '.*/[aLO].*\.tiny\.png'` \
		uf.png

cf.png: $(STATICIMAGES:.png=.tiny.png)
	montage -tile $(FRISE_WIDTH)x$(FRISE_HEIGHT) \
		-mode Concatenate -background white -adjoin \
		`find . -regex '.*/[bcdij].*\.tiny\.png'` \
		cf.png

lf.png: $(STATICIMAGES:.png=.tiny.png)
	montage -tile $(FRISE_WIDTH)x$(FRISE_HEIGHT) \
		-mode Concatenate -background white -adjoin \
		`find . -regex '.*/[mpsty].*\.tiny\.png'` \
		lf.png

SUFFIXES = .png .small.png .tiny.png

# the directory check is needed for VPATH build
.png.small.png:
	dir=`dirname $@`; \
	if [ ! -d $$dir ]; then \
		mkdir -p $$dir; \
	fi
	convert -geometry $(SMALL_WIDTH)x$(SMALL_HEIGHT) $< $@

# the directory check is needed for VPATH build
.png.tiny.png:
	dir=`dirname $@`; \
	if [ ! -d $$dir ]; then \
		mkdir -p $$dir; \
	fi
	convert -geometry $(TINY_WIDTH)x$(TINY_HEIGHT) $< $@


