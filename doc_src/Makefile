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

include ../Makefile.config

# The list of HTML source files
HTML_SRC_FILES	= advi-index.html advi-fra.html advi-eng.html \
		  advi-faq-eng.html advi-faq-fra.html \
		  advi-screen-shots-fra.html advi-screen-shots-eng.html

# The list of MAN source files
MAN_SRC_FILES	= advi.man

# The list of build HTML files
HTML_FILES	= $(HTML_SRC_FILES:.html=.htm)

# The list of build MAN files
MAN_FILES	= $(MAN_FILES:.man=.1)

all: $(HTML_FILES) $(MAN_FILES) ../Makefile.config

clean:
	$(RM) $(HTML_FILES) $(MAN_FILES)

.SUFFIXES:
.SUFFIXES: .htm .html .man .m

.html.htm:
	$(HTMLC) $(HTMLC_FLAGS) -f $< -t $@

.man.1:
	$(HTMLC) $(HTMLC_FLAGS) -f $< -t $@
