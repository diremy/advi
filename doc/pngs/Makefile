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

# Makefile to generate the thumbnails images for the WEB site
# Pierre Weis

include ../../Makefile.config

MKTHUMBNAIL=TOOLS/mkthumbnail
MKFRISE=TOOLS/mkfrise
RM=/bin/rm -f

DIRS=basics basics/cash \
prosper/Join prosper/LL \
seminar/a14 seminar/clock \
slitex/advi slitex/caml slitex/simplistic slitex/skin_detection \
test

all: f.png

f.png:
	$(MKFRISE)

clean:
	for i in $(DIRS); do \
	$(RM) $$i/thumb_*; \
	done; \
	$(RM) thumb_*; \
	$(RM) f.png
