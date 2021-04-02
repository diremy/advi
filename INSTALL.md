PREREQUISITES
=============

You need Objective Caml 4.11 or higher to compile the sources,
including the following packages, available under opam:

        "dune" {>= "2.5"}
        "graphics" {>= "5.1.1"}
        "camlimages" {>= "5.0.4"}

This version is available as an opam package. 

You also need the `kpsewhich' utility of the `kpathsea' library
provided by many TeX distributions.

For bitmap based image inclusion using \includegraphics of 
the graphics macro package, you require the camlimages library,
with version 5.0.4, available under opam. 

To display postscript based drawings by PsTricks macro package,
you need ghostscript (http://www.ghostscript.com). You need
the version 7.05 or later for the correct synchronization of
drawing of the TeX glyphs and postscript graphics.

COMPILATION UNDER OPAM
======================

This should install automatically as an opam package.

The default installation, does not install the latex style files.
To do so you need to run the command

        advi-latex-files --install
    
after the opam package installation. 


COMPILATION AND INSTALLATION
============================

Use the standard procedure:

        make && sudo make install
    
Then you also need to install the latex source files: 

        make install-latex-files
    
Alternatively, you may:

- Install the binary by hand, by simply copying it (them) to the
   appropriate directory.
- Copy the files advi.sty to a place where it is a visible latex package.
  (kpsewhich advi.sty should succeed from where LaTeX is run)
- Copy the file advi.pro to a place where is it a visible PS header.
   (kpsewhich advi.pro should succeed from where advi is run)


JAPANESE USERS
==============

There is a configuration file for mapping Japanese TeX font names
to Japanese True Type fonts called "jpfonts.conf". It is placed in
the advi library directory (under opam prefix). 
If you want to use different TrueType fonts, you can edit this file,
or put your own jpfonts.conf in ~/.advi/ .
