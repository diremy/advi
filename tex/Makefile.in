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
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
#  Based on Mldvi by Alexandre Miquel.                                #
#*********************************************************************#

# $Id$

include ../Makefile.config

STYFILES = advi-annot.sty advi-slides.sty argv.sty superpose.sty \
    advi-graphicx.sty advi.sty bubble.sty xwindows-colors.sty

EPSFILES = advilogo.eps bar.eps caml.eps

install:
	$(INSTALL) -m 644 $(EPSFILES) $(STYFILES) $(DESTDIR)$(ADVI_LOC)

