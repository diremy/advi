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
# Pierre Weis                                                         #
#                                                                     #
#*********************************************************************#

# The Makefile for the slitex skin_detection sub-directory example.

# $Id$

# To view the demos, use ../../../advi.opt file.dvi

all: skin_detection.dvi see

edit:
	emacs skin_detection.tex

see:
	advi skin_detection.dvi

clean:
	/bin/rm -f *~ *.txt *.log *.aux *.dvi

.SUFFIXES: .tex .dvi

.tex.dvi:
	latex $<

skin_detection.dvi: skin_detection.tex



