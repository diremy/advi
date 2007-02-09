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
nobase_png_DATA = $(IMAGES)
IMAGES = f.png \
	  basics/adviwait.png \
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

EXTRA_DIST = $(IMAGES)

# Makefile to generate the thumbnails images for the WEB site
# Pierre Weis
#

MKTHUMBNAIL=TOOLS/mkthumbnail
MKFRISE=TOOLS/mkfrise

DIRS=basics basics/cash \
prosper/Join prosper/LL \
seminar/a14 seminar/clock \
slitex/advi slitex/caml slitex/simplistic slitex/skin_detection \
test

f.png:
	$(MKFRISE)

clean:
	for i in $(DIRS); do \
	$(RM) $$i/thumb_*; \
	done; \
	$(RM) thumb_*; \
	$(RM) f.png
