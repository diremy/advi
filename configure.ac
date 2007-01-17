# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.59)
AC_INIT(advi, 1.7.0, Pierre.Weis@inria.fr)
BUILD_DATE=`date +%Y-%m-%d`
AC_SUBST(BUILD_DATE)
AM_INIT_AUTOMAKE([foreign])

# Checking ocaml
AC_PROG_OCAML()
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLVERSION)

# Checks other programs
AC_PROG_CC

# check for ocamlfind
AC_PATH_PROG(OCAMLFIND, ocamlfind)
if test -z "$OCAMLFIND"; then
    AC_MSG_ERROR([ocamlfind not found, check your PATH])
fi

# Checking CamlImages
AC_MSG_CHECKING([for camlimages])
CAMLIMAGES_VERSION=`$OCAMLFIND query -format '%v' camlimages`
if test -z "$CAMLIMAGES_VERSION"; then
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([you need camlimages, with minimal 3.0.0 version])
else
    AC_MSG_RESULT([yes])
fi

CAMLIMAGES_INCLUDES=`$OCAMLFIND query -i-format -separator ' ' -recursive camlimages`
CAMLIMAGES_LIBS_NATIVE=`$OCAMLFIND query -a-format -separator ' ' -recursive -predicates native camlimages`
CAMLIMAGES_LIBS_BYTE=`$OCAMLFIND query -a-format -separator ' ' -recursive -predicates byte camlimages`

AC_SUBST(CAMLIMAGES_INCLUDES)
AC_SUBST(CAMLIMAGES_LIBS_NATIVE)
AC_SUBST(CAMLIMAGES_LIBS_BYTE)

# Checking LablTk
AC_MSG_CHECKING([for labltk])
LABLTK_VERSION=`$OCAMLFIND query -format '%v' labltk`
if test -z "$LABLTK_VERSION"; then
    AC_MSG_RESULT([no])
else
    AC_MSG_RESULT([yes])
fi

LABLTK_INCLUDES=`$OCAMLFIND query -i-format -separator ' ' -recursive labltk`
LABLTK_LIBS_NATIVE=`$OCAMLFIND query -a-format -separator ' ' -recursive -predicates native labltk`
LABLTK_LIBS_BYTE=`$OCAMLFIND query -a-format -separator ' ' -recursive -predicates byte labltk`

AC_SUBST(LABLTK_INCLUDES)
AC_SUBST(LABLTK_LIBS_NATIVE)
AC_SUBST(LABLTK_LIBS_BYTE)

# conditional install target
AC_MSG_CHECKING([wether to build either native or bytecode version])
if test -n "$OCAMLOPT" && test -n "$CAMLIMAGES_LIBS_NATIVE"; then
    AC_MSG_RESULT([native])
    build_opt=true
else
    AC_MSG_RESULT([bytecode])
    build_opt=false
fi
AM_CONDITIONAL(BUILD_OPT, test x$build_opt = xtrue)

# checking for X
AC_PATH_XTRA
AC_SUBST(X_LIBS)
AC_SUBST(X_PRE_LIBS)
AC_SUBST(X_EXTRA_LIBS)
AC_SUBST(X_CFLAGS)

# Checking for xinerama support
have_xinerama="false"
AC_CHECK_HEADER(
    [X11/extensions/Xinerama.h],
    [AC_CHECK_LIB(
	[Xinerama],
	[XineramaQueryScreens],
	[have_xinerama="true"
	 XINERAMA_CFLAGS="-DHAVE_XINERAMA"
	 XINERAMA_LIBS="-lXinerama"]
    )]
)
AC_SUBST(XINERAMA_CFLAGS)
AC_SUBST(XINERAMA_LIBS)

# Checking TeX 
AC_PATH_PROG(KPSEXPAND, kpsexpand)
if test -z "$KPSEXPAND"; then
    AC_MSG_ERROR([kpsexpand not found, check your PATH])
fi

AC_MSG_CHECKING([TeX root path])
TEXMFMAIN=`$KPSEXPAND '$TEXMFMAIN'`
AC_MSG_RESULT([$TEXMFMAIN])
if test -z "$TEXMFMAIN"; then
    AC_MSG_ERROR([no TeX root path found, check your setup])
fi

AC_MSG_CHECKING([LaTeX root path])
if test -d "$TEXMFMAIN/tex/latex"; then
    AC_MSG_RESULT([$TEXMFMAIN/tex/latex])
    ADVI_LOC="$TEXMFMAIN/tex/latex/advi"
else 
    AC_MSG_RESULT([not found])
    AC_MSG_ERROR([no LaTeX root path found, check your setup])
fi
AC_SUBST(TEXMFMAIN)
AC_SUBST(ADVI_LOC)

# Checking LaTeX
AC_PATH_PROG([LATEX], [latex])
if test -z "$LATEX"; then
    AC_MSG_ERROR([latex not found, check your PATH])
fi
# TODO: check LaTeX environment

# Checking dvips
AC_PATH_PROG([DVIPS], [dvips])
if test -z "$DVIPS"; then
    AC_MSG_ERROR([dvips not found, check your PATH])
fi
AC_PATH_PROG([DVIPDFM], [dvipdfm])

# Checking hevea
AC_PATH_PROG([HEVEA], [hevea])
AC_PATH_PROG([PDFIMAGEN], [pdfimagen])

# Checking htmlc
AC_PATH_PROG([HTMLC], [htmlc])

# conditional documentation targets
ALL_DOC_FILES="\$(LATEX_FILES)"
if test -n "$HEVEA"; then
    ALL_DOC_FILES="$ALL_DOC_FILES \$(HEVEA_FILES)"
fi

if test -n "$HTMLC"; then
    ALL_DOC_FILES="$ALL_DOC_FILES \$(HTMLC_FILES)"
fi
AC_SUBST(ALL_DOC_FILES)

# Checking gs
HAVE_GS="false"
AC_PATH_PROG(GS, gs) 

if test -n "$GS" ; then
    AC_MSG_CHECKING([if gs version >= 6.52]) 
    gs_version=`$GS -v | awk '/Ghostscript/ {print $3}'`
    gs_major_version=`echo $gs_version | cut -d. -f1`
    gs_minor_version=`echo $gs_version | cut -d. -f2`

    if expr                                \
	\( $gs_major_version \> 6 \)       \
	\|                                 \
	\(                                 \
	    \( $gs_major_version \= 6 \)   \
	    \&                             \
	    \( $gs_minor_version \>= 52 \) \
	\) > /dev/null; then
	AC_MSG_RESULT([yes])
	HAVE_GS="true"
    else
	AC_MSG_RESULT([no])
	GS=""
    fi
fi
AC_SUBST(HAVE_GS)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT()

cat <<EOF
Configuration summary

Language:    $OCAMLC version $OCAMLVERSION
Camlimages:  $HAVE_CAMLIMAGES
LablTk:      $HAVE_LABLTK
GS:          $HAVE_GS
Resources:   $ADVI_LOC/
Hevea:       $HEVEA
Xinerama:    $have_xinerama
EOF
