#!/bin/bash

function gendef () {
    name="$1"
    value="$2"
    echo "#define ${name} ${value}"
}

package_version='%%VERSION%%'

build_date=$(date +%Y-%m-%d)

have_gs=true

gs_path=$(which gs)

function opamvar () {
    name="$1"
    var="$2"
    suffix="$3"
    unset opamdir
    opamdir=$(opam config var ${var})
    case "${opamdir}" in "") exit 1;; esac
    gendef ${name} \"${opamdir}/${suffix}\"
}


case "$#" in
    0)
        exit 1;;
    *)
        target="$1" 
        exec >${target} ;;
esac



opamvar ADVI_ETCDIR etc advi
opamvar ADVI_DOCDIR doc advi
opamvar ADVI_TEXDIR share texmf/tex/latex/advi

gendef "PACKAGE_VERSION" \"${package_version}\"
gendef "BUILD_DATE" \"${build_date}\"
gendef "HAVE_GS" ${have_gs}
gendef "GS_PATH" \"${gs_path}\"

gendef HAVE_CAMLIMAGES true
