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

# Makefile for HTML files generation.

# From a list of source files (.html files) generates the
# corresponding objects (.htm files) using Htmlc.

# Usage: fill in the HTMLC and FILES variables below, then
# make (or make all) will rebuild the whole thing
# make clean will remove all the generated files

# The Htmlc compiler with its include path options
HTMLC=htmlc -I Includes -env Includes/env

# The list of source files
FILES=advi-index.html advi-fra.html advi-eng.html \
advi-faq-eng.html advi-faq-fra.html

######## This part should automatically handle the generation of
######## object files

# The list of object files
OBJS=$(FILES:.html=.htm)

all: $(OBJS)

clean:
	/bin/rm -f $(OBJS)
	cd Includes; /bin/rm -f *~

.SUFFIXES:
.SUFFIXES: .htm .html

.html.htm:
	$(HTMLC) -f $< -t $@
