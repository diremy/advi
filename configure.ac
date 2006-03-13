dnl Process this file with autoconf to produce a configure script.
AC_PREREQ(2.59)
AC_INIT(advi, 1.7.0, Pierre.Weis@inria.fr)
PACKAGE_DATE=`date +%Y-%m-%d`

dnl try to find ocaml
CAML=ocaml
AC_PATH_PROG(PATH_CAML, $CAML, "nope") 
if test "$PATH_CAML" = "nope"; then
  echo "***************** Error *******************"
  echo "ocaml not found."
  echo "Install the latest ocaml system"
  echo "and check your PATH contains ocamlc."
  echo "*******************************************"
  exit -1
fi

dnl try to find your compiler package
AC_MSG_CHECKING(your compiler version)

CAMLDIR=`"$CAML"c -v | grep "^Standard" | sed "s/.*: *//"`
CAMLVERSION=`"$CAML"c -v | grep "compiler" | sed 's/.* version //' | sed 's/ .*$//'`

AC_MSG_RESULT(version $CAMLVERSION at $CAMLDIR)

CAMLOPT="$CAML"opt
  
dnl TeX 

AC_PATH_PROG(PATH_KPSEXPAND, kpsexpand, "nope") 
if test "$PATH_KPSEXPAND" = "nope"; then
  echo "**************** Warning ******************"
  echo "kpsexpand command not found." 
  echo "We use /usr/share/texmf as the root directory"
  echo "of TeX system"
  echo "*******************************************"
  TEXMFMAIN="/usr/share/texmf"
else
  AC_MSG_CHECKING(TeX root path)
  TEXMFMAIN=`kpsexpand '$TEXMFMAIN'`
  AC_MSG_RESULT($TEXMFMAIN)
fi

AC_PATH_PROG(PATH_KPSEWHICH, kpsewhich, "nope") 
if test "$PATH_KPSEWHICH" = "nope"; then
  echo "**************** Warning ******************"
  echo "kpsewhich command not found." 
  echo "We use a $PATH dependant kpsewhich command"
  echo "*******************************************"
  PATH_KPSEWHICH="kpsewhich"
fi

dnl Gzip

AC_PATH_PROG(PATH_GZIP, gzip, "nope") 
if test "$PATH_GZIP" = "nope"; then
  echo "**************** Warning ******************"
  echo "gzip command not found." 
  echo "We use a $PATH dependant gzip command"
  echo "*******************************************"
  PATH_GZIP=""
fi

dnl Checks for libraries.

dnl CamlImages

CAMLIMAGESDIR=$CAMLDIR/camlimages

AC_MSG_CHECKING(for camlimages)
AC_ARG_WITH(camlimages, [  --with-camlimages=dir      specify camlimages library directory],
  if test "$withval" != "no" ; then  
	CAMLIMAGESDIR=$withval
  else
	AC_MSG_RESULT(no)
	CAMLIMAGESDIR=nope
  fi)

dnl Camlimages version

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

dnl LablTk

LABLTKDIR=$CAMLDIR/labltk

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

dnl Checking gs:

PATH_GS="nope"
AC_ARG_WITH(gs, [  --with-gs=path          specify gs path],
  if test "$withval" != "no" ; then  
	PATH_GS=$withval
  else
	PATH_GS="nope"
  fi)

if test "X$PATH_GS" = "Xnope"; then
  AC_PATH_PROG(PATH_GS, gs, "nope") 
fi

dnl check gs really exists and its version
if test -f "$PATH_GS" ; then
  AC_MSG_CHECKING(for gs version) 
  HAVE_GS="true"
  GS_VERSION=`$PATH_GS -v | grep Ghostscript | sed -e 's/.*Ghostscript \([[0-9\.]]*\).*/\1/'`
  GS_VERSION_INT=`echo $GS_VERSION | sed -e 's/\.//'`
  if test `expr $GS_VERSION_INT \>= 6.52` != "1"; then
    AC_MSG_RESULT(too old)
    GS_UPTO_DATE="*** TOO OLD ***"
    echo "**************** Warning ********************"
    echo "Your Ghostscript $GS_VERSION may be too old  "
    echo "You may need Ghostscript version 6.52 or higher"
    echo "for the correct synchronization of postscript"
    echo "and TeX graphics                             "
    echo "*********************************************"
  else
    GS_UPTO_DATE=""	
    AC_MSG_RESULT(ok)
  fi
else
  AC_MSG_RESULT(no)
  HAVE_GS="false"
fi

dnl Check for libraries: END

AC_PATH_PROG(PATH_CAMLOPT, $CAMLOPT, "nope")
if test "$PATH_CAMLOPT" = "nope"; then
  echo "**************** Warning ******************"
  echo "The ocaml native code compiler is not found"
  echo "Only the bytecode version of Advi will be"
  echo "compiled and installed. We strongly recommend"
  echo "to install the ocamlopt compiler if available"
  echo "in your OS."
  echo "*******************************************"
  INSTALLTARGET="i_want_opt"
else
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
fi

AC_PATH_XTRA

dnl Check for xinerama support

HAVE_XINERAMA="false"
AC_CHECK_HEADER(X11/extensions/Xinerama.h,
HAVE_XINERAMA="true";
XINERAMA_FLAG="-DHAVE_XINERAMA";
XINERAMA_LIBS="-lXinerama -lXext")

WITH_X="$X_LIBS $X_PRE_LIBS $XINERAMA_LIBS -lX11 $X_EXTRA_LIBS"

dnl Checking hevea:

PATH_HEVEA="nope"
AC_ARG_WITH(hevea, [  --with-hevea=path          specify hevea path],
  if test "$withval" != "no" ; then  
	PATH_HEVEA=$withval
  else
	PATH_HEVEA="nope"
  fi)

if test "X$PATH_HEVEA" = "Xnope"; then
  AC_PATH_PROG(PATH_HEVEA, hevea, "nope") 
fi

if test "$PATH_HEVEA" = "nope"; then
  HAVE_HEVEA=false
else
  HAVE_HEVEA=true
fi

dnl Resources location
ADVI_LOC=`pwd`

dnl Substitute variables

AC_SUBST(CAML)
AC_SUBST(CAMLDIR)
AC_SUBST(CAMLVERSION)

AC_SUBST(CAMLIMAGESDIR)
AC_SUBST(CAMLIMAGESCONFIGINCLUDE)
AC_SUBST(HAVE_CAMLIMAGES)
AC_SUBST(CAMLIMAGESLIBS)
AC_SUBST(HAVE_GS)
AC_SUBST(PATH_GS)
AC_SUBST(TEXMFMAIN)
AC_SUBST(PATH_KPSEWHICH)
AC_SUBST(PATH_GZIP)
AC_SUBST(HAVE_LABLTK)
AC_SUBST(LABLTKDIR)
AC_SUBST(LABLTKLIB)
AC_SUBST(INSTALLTARGET)
AC_SUBST(HAVE_XINERAMA)
AC_SUBST(XINERAMA_FLAG)
AC_SUBST(PATH_HEVEA)
AC_SUBST(HAVE_HEVEA)

dnl BUILD OUR OWN LIBDIR
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

AC_SUBST(PACKAGE_VERSION)
AC_SUBST(PACKAGE_DATE)
AC_SUBST(ADVI_LOC)
AC_SUBST(WITH_X)
AC_SUBST(HAVE_GS)
AC_OUTPUT(Makefile)

echo "------------------------------------------------- Configuration finished"
echo
echo "             Configuration summary"
echo
echo "Language:    $CAML version $CAMLVERSION"
echo "Camlimages:  $HAVE_CAMLIMAGES"
echo "LablTk:      $HAVE_LABLTK"
echo "GS:          $GS_UPTO_DATE version $GS_VERSION at $PATH_GS"
echo "Resources:   $ADVI_LOC/"
echo "Hevea:       $PATH_HEVEA"
echo "Xinerama:    $HAVE_XINERAMA"		
echo "------------------------------------------------------------------------"
