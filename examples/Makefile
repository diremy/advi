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

# The Makefile for the various example subdirectories.

# $Id$

SUBDIRS=basics prosper seminar slitex

all:
	for i in $(SUBDIRS); do \
	 echo $$i; \
	 cd $$i; $(MAKE) all; cd ..; \
	done

clean:
	for i in $(SUBDIRS); do \
	 cd $$i; $(MAKE) clean; cd ..; \
	done
