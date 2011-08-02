PREREQUISITES
=============

You need Objective Caml 3.11 or higher to compile the sources.

You also need the `kpsewhich' utility of the `kpathsea' library
provided by many TeX distributions.

For bitmap based image inclusion using \includegraphics of 
the graphics macro package, you require the camlimages library,
with version 4.0.0, available at 

	http://gallium.inria.fr/camlimages/

You need the findlib utility to automatically detect installed
ocaml libraries configuration, available at

        http://www.camlcity.org/archive/programming/findlib.html

To display postscript based drawings by PsTricks macro package,
you need ghostscript (http://www.ghostscript.com). You need
the version 7.05 or later for the correct synchronization of
drawing of the TeX glyphs and postscript graphics.

COMPILATION AND INSTALLATION
============================

Use the standard procedure:
./configure && make && sudo make install

Use ./configure --help for various options

Alternatively:
* Install the binary by hand, by simply copying it (them) to the
   appropriate directory.
* Copy the files advi.sty to a place where it is a visible latex package.
  (kpsewhich advi.sty should succeed from where LaTeX is run)
* Copy the file advi.pro to a place where is it a visible PS header.
   (kpsewhich advi.pro should succeed from where advi is run)

Generating build scripts
------------------------
This is only needed if you're using CVS version directly.

  * make sure ocaml autoconf macros, available from bazar-ocaml/autoconf
   directory, are present in aclocal macro search path, by any of the following 
   solution:
   - exec before invoking autoreconf:
       export ACLOCAL='aclocal -I <directory>' 
   - set up a dirlist file in automake macro directory, as explained in aclocal
     documentation
  * run autoreconf --install

JAPANESE USERS
==============

There is a configuration file for mapping Japanese TeX font names
to Japanese True Type fonts called "jpfonts.conf". It is placed in
the advi library directory (usually /usr/local/lib/advi/). 
If you want to use different TrueType fonts, you can edit this file,
or put your own jpfonts.conf in ~/.advi/ .
