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

# make a big tar ball
ADKVERSION=1.6.0
CAMLIMAGESVERSION=2.2

FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal

distribute: clean
	(cd adk; \
	   tar zxvf $(FTPSITEDIR)/caml-light/bazar-ocaml/camlimages-$(CAMLIMAGESVERSION).tgz; \
	   tar zxvf $(FTPSITEDIR)/advi/advi-$(ADKVERSION).tgz)
	tar zcvf adk-$(ADKVERSION).tgz --exclude '*~' --exclude .cvsignore --exclude CVS adk
	cp adk-$(ADKVERSION).tgz $(FTPSITEDIR)/advi

clean:
	rm -f adk-$(ADKVERSION).tgz
	(cd adk; \
	   /bin/rm -rf camlimages* advi*)
