#*********************************************************************#
#                                                                     #
#                                Advi                                 #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 2003 Institut National de Recherche en Informatique et   #
#  en Automatique.  Distributed only by permission.                   #
#                                                                     #
#*********************************************************************#

# $Id$

# Makefile for HTML and man files generation.

# From a list of source files (.html files) generates the
# corresponding objects (.htm files) using Htmlc.

# Usage: fill in the HTMLC, HFILES, and MFILES variables below, then
# make (or make all) will rebuild the whole thing
# make clean will remove all the generated files

# The Htmlc compiler with its include path options
HTMLC=htmlc -I Includes -env Includes/env

# The list of HTML source files
HFILES=advi-index.html advi-fra.html advi-eng.html \
advi-faq-eng.html advi-faq-fra.html \
advi-screen-shots-fra.html advi-screen-shots-eng.html

# The list of MAN source files
MFILES=advi.man

######## This part should automatically handle the generation of
######## object files

# The complete list of object files
HOBJS=$(HFILES:.html=.htm)
MOBJS=$(MFILES:.man=.m)
OBJS=$(HOBJS) $(MOBJS)

all: $(OBJS)

clean:
	/bin/rm -f $(OBJS)
	cd Includes; /bin/rm -f *~

.SUFFIXES:
.SUFFIXES: .htm .html .man .m

.html.htm:
	$(HTMLC) -f $< -t $@

.man.m:
	$(HTMLC) -f $< -t $@

