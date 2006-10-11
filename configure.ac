# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.59)
AC_INIT(advi, 1.7.0, Pierre.Weis@inria.fr)
BUILD_DATE=`date +%Y-%m-%d`

# Checking ocaml
AC_PROG_OCAML()

  
# Checking TeX 
AC_PATH_PROG(KPSEXPAND, kpsexpand)
if test -z "$KPSEXPAND"; then
    AC_MSG_WARN([using /usr/share/texmf as the root directory of TeX system])
    TEXMFMAIN="/usr/share/texmf"
else
    AC_MSG_CHECKING(TeX root path)
    TEXMFMAIN=`$KPSEXPAND '$TEXMFMAIN'`
    AC_MSG_RESULT($TEXMFMAIN)
fi

# Checking CamlImages
CAMLIMAGESDIR=$OCAMLLIB/camlimages

AC_MSG_CHECKING(for camlimages)
AC_ARG_WITH(
    camlimages,
    AC_HELP_STRING(
	[--with-camlimages=dir],
	[specify camlimages library directory]
    ),
    if test "$withval" != "no" ; then  
	CAMLIMAGESDIR=$withval
    else
	AC_MSG_RESULT(no)
	CAMLIMAGESDIR=nope
    fi
)

if test -f $CAMLIMAGESDIR/ci_ps.cma; then
    AC_MSG_RESULT(found)
    CAMLIMAGES_VERSION=`grep VERSION $CAMLIMAGESDIR/Makefile.config | sed -e 's/VERSION=//'`
    if test `expr $CAMLIMAGES_VERSION \>= 2.11` = "1"; then
	HAVE_CAMLIMAGES="true"
	CAMLIMAGESCONFIGINCLUDE="include $CAMLIMAGESDIR/Makefile.config"
	CAMLIMAGESLIBS='$(WITH_CAMLIMAGES:.cma=)'
    else
	AC_MSG_RESULT(found, but bit too old)
	echo "***************** Error *******************"
	echo "camlimages is found in the directory \"$CAMLIMAGESDIR\"",
	echo "but bit too old. (make sure install the latest version 2.10)"
	echo "*******************************************"
	HAVE_CAMLIMAGES="false"
	CAMLIMAGESLIBS=""
	exit 1
    fi
else
    AC_MSG_RESULT(not found, or too old)
    echo "***************** Error *******************"
    echo "camlimages is not found or incomplete in the directory \"$CAMLIMAGESDIR\""
    echo "(make sure install the latest version 2.10)"
    echo "*******************************************"
    HAVE_CAMLIMAGES="false"
    CAMLIMAGESLIBS=""
    exit 1
fi

if test -n "$OCAMLOPT"; then
    if test -f "$CAMLIMAGESDIR/ci_ps.cmxa"; then
	INSTALLTARGET="advi.bin"
    else
	echo "**************** Warning ******************"
	echo "You have the ocamlopt compiler, but you have no"
	echo "native-code version of camlimages library installed."
	echo "Without it, we cannot compile the native-code"
	echo "version of advi, which we strongly recommend."
	echo "*******************************************"
	INSTALLTARGET="i_want_opt"
    fi
else
    INSTALLTARGET="i_want_opt"
fi

# Checking LablTk
LABLTKDIR=$OCAMLLIB/labltk

AC_MSG_CHECKING(for labltk)

if test -f $LABLTKDIR/cTk.cmi; then
    AC_MSG_RESULT(found)
    HAVE_LABLTK="true"
    LABLTKLIB=$LABLTKDIR/labltk.cma
else
    AC_MSG_RESULT(no)
    HAVE_LABLTK="false"
    echo "***************** Warning *******************"
    echo "Your labltk library is too old. Use the latest one,"
    echo "found in the O'Caml CVS tree"
    echo "*******************************************"
fi

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


AC_PATH_XTRA

# Checking for xinerama support
HAVE_XINERAMA="false"
AC_CHECK_HEADER(X11/extensions/Xinerama.h,
HAVE_XINERAMA="true";
XINERAMA_FLAG="-DHAVE_XINERAMA";
XINERAMA_LIBS="-lXinerama -lXext")

WITH_X="$X_LIBS $X_PRE_LIBS $XINERAMA_LIBS -lX11 $X_EXTRA_LIBS"

# Checking hevea
AC_PATH_PROG([HEVEA], [hevea])

# Resources location
ADVI_LOC=`pwd`
myprefix=$prefix
myexec_prefix=$exec_prefix
mylibdir=$libdir

test "x$myprefix" = xNONE && myprefix=$ac_default_prefix
test "x$myexec_prefix" = xNONE && myexec_prefix=${myprefix}
mylibdir=$libdir
test "x$libdir" = 'x${exec_prefix}/lib' && mylibdir="$myexec_prefix/lib"

if test -d $TEXMFMAIN/tex/latex; then
    ADVI_LOC=$TEXMFMAIN/tex/latex/advi
else 
    ADVI_LOC=$mylibdir/advi
fi

# conditional targets
ALL_DOC_FILES="\$(OTHER_FILES)"
if test -n "$HEVEA"; then
    ALL_DOC_FILES="$ALL_DOC_FILES \$(HEVEA_FILES)"
fi

# Substitute variables
AC_SUBST(OCAMLLIB)
AC_SUBST(CAMLIMAGESDIR)
AC_SUBST(CAMLIMAGESCONFIGINCLUDE)
AC_SUBST(HAVE_CAMLIMAGES)
AC_SUBST(CAMLIMAGESLIBS)
AC_SUBST(TEXMFMAIN)
AC_SUBST(HAVE_LABLTK)
AC_SUBST(LABLTKDIR)
AC_SUBST(LABLTKLIB)
AC_SUBST(INSTALLTARGET)
AC_SUBST(HAVE_XINERAMA)
AC_SUBST(XINERAMA_FLAG)
AC_SUBST(BUILD_DATE)
AC_SUBST(ADVI_LOC)
AC_SUBST(WITH_X)
AC_SUBST(HAVE_GS)
AC_SUBST(ALL_DOC_FILES)

AC_OUTPUT(Makefile Makefile.config doc/Makefile)

cat <<EOF
Configuration summary

Language:    $OCAMLC version $OCAMLVERSION
Camlimages:  $HAVE_CAMLIMAGES
LablTk:      $HAVE_LABLTK
GS:          $HAVE_GS
Resources:   $ADVI_LOC/
Hevea:       $HEVEA
Xinerama:    $HAVE_XINERAMA
EOF
