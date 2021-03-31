#!/bin/bash

function valdef () {
    name="$1"
    value="$2"
    echo "  -e 's|@${name}@|${value}|g' \\"
}

function strdef () {
    name="$1"
    value="$2"
    echo "  -e 's|@${name}@|${value}|g' \\"
}

function error () {
    echo $* 1>&2
    exit 1
}    

package_version='%%VERSION%%'

build_date=$(date +%Y-%m-%d)

if gs_path=$(which gs)
then have_gs=true
else have_gs=false
fi


function opamvar () {
    name="$1"
    var="$2"
    suffix="$3"
    unset opamdir
    opamdir=$(opam config var ${var})
    case "${opamdir}" in "") exit 1;; esac
    case "$suffix" in
        "") strdef ${name} "${opamdir}" ;;
        *) strdef ${name} "${opamdir}/${suffix}" ;;
    esac
}

case "$#" in
    0)
        exit 1;;
    *)
        target="$1" 
        exec >${target} ;;
esac

libdir=${exec_prefix}/lib
datadir=${prefix}/share

# TEX ROOTS 
texdir=$(kpsexpand '$TEXMFMAIN')
if test -z "$texdir"
then error 'no TeX root path found, check your setup'
fi

# LATEX ROOTS 
if test -d "$texdir/tex/latex"; then
    # this ugly stuff, shamelessly stolen from lispdir computation, is supposed
    # to find latex resources installation directory expressed in term of prefix
    # the ugly @<:@^\/@:>@ Is meant to express [^\/] regexp
    latexdir=`echo $texdir/tex/latex | sed -n \
	-e 's,/$,,' \
        -e '/.*\/lib\/[^\/]*\/tex\/latex$/{s,.*/lib/\([^\/]*\/tex\/latex\)$,${libdir}/\1,;p;q;}' \
        -e '/.*\/share\/[^\/]*\/tex\/latex$/{s,.*/share/\([^\/]*\/tex\/latex\),${datadir}/\1,;p;q;}'`
else 
    error 'no LaTeX root path found, check your setup'
fi

if test -z "$latexdir"
then error 'No LaTeX root path found, check your setup'
fi

echo '#/bin/bash'
echo
echo 'src="$1"'
echo 'dest="$2"'
echo
echo 'LC_CTYPE=C && LANG=C sed \'

opamvar ADVI_MANDIR man 
opamvar ADVI_ETCDIR etc advi
opamvar ADVI_DOCDIR doc advi
opamvar ADVI_TEXDIR share advi

strdef "TEXDIR" ${texdir}
strdef "LATEXDIR" ${latexdir}

strdef "PACKAGE_VERSION" ${package_version}
strdef "BUILD_DATE" ${build_date}
valdef "HAVE_GS" ${have_gs}
strdef "GS_PATH" ${gs_path}

valdef HAVE_CAMLIMAGES true

echo ' $src > $dest '

