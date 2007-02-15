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
texdir=`$KPSEXPAND '$TEXMFMAIN'`
AC_MSG_RESULT([$texdir])
if test -z "$texdir"; then
    AC_MSG_ERROR([no TeX root path found, check your setup])
fi

AC_MSG_CHECKING([LaTeX root path])
if test -d "$texdir/tex/latex"; then
    AC_MSG_RESULT([$texdir/tex/latex])
    # this ugly stuff, shamelessly stolen from lispdir computation, is supposed
    # to find latex resources installation directory expressed in term of prefix
    # the ugly @<:@^\/@:>@ is meant to express [^\/] regexp
    latexdir=`echo $texdir/tex/latex | sed -n \
	-e 's,/$,,' \
	-e '/.*\/lib\/@<:@^\/@:>@*\/tex\/latex$/{s,.*/lib/\(@<:@^\/@:>@*\/tex\/latex\)$,${libdir}/\1,;p;q;}' \
	-e '/.*\/share\/@<:@^\/@:>@*\/tex\/latex$/{s,.*/share/\(@<:@^\/@:>@*\/tex\/latex\),${datadir}/\1,;p;q;}'`
else 
    AC_MSG_RESULT([not found])
    AC_MSG_ERROR([no LaTeX root path found, check your setup])
fi
AC_SUBST(latexdir)

# Checking tools needed for building documentation
# only latex is mandatory, other are optional
# as documentation is distributed anyway
AC_PATH_PROG([LATEX], [latex])
if test -z "$LATEX"; then
    AC_MSG_ERROR([latex not found, you won't be able to rebuild documention])
fi
AC_PATH_PROG([DVIPS], [dvips])
if test -z "$DVIPS"; then
    AC_MSG_WARN([dvips not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_DVIPS, [test -n "$DVIPS"])
AC_PATH_PROG([DVIPDFM], [dvipdfm])
if test -z "$DVIPDFM"; then
    AC_MSG_WARN([dvips not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_DVIPDFM, [test -n "$DVIPDFM"])
AC_PATH_PROG([HEVEA], [hevea])
if test -z "$HEVEA"; then
    AC_MSG_WARN([hevea not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_HEVEA, [test -n "$HEVEA"])
AC_PATH_PROG([HTMLC], [htmlc])
if test -z "$HTMLC"; then
    AC_MSG_WARN([htmlc not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_HTMLC, [test -n "$HTMLC"])
AC_PATH_PROG([CONVERT], [convert])
if test -z "$CONVERT"; then
    AC_MSG_WARN([convert not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_CONVERT, [test -n "$CONVERT"])
AC_PATH_PROG([MONTAGE], [montage])
if test -z "$MONTAGE"; then
    AC_MSG_WARN([montage not found, you won't be able to rebuild documention])
fi
AM_CONDITIONAL(HAVE_MONTAGE, [test -n "$MONTAGE"])

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

AC_CONFIG_FILES([Makefile
		 src/Makefile
		 doc/Makefile
		 doc/pngs/Makefile
		 tex/Makefile])
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
