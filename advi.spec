Release: 1
Version: 1.0.0
Name: advi
Summary: Active-DVI Presenter
Source: ftp://ftp.inria.fr/INRIA/Projects/cristal/advi/advi-1.0.0.tar.gz
Group: Applications/Publishing
URL: http://pauillac.inria.fr/advi/
Copyright: LGPL
Packager: <Jun.Furuse@inria.fr>
Provides: advi

%description 
Active-DVI is a presentation graphics program to provide the highest
quality slides presentation experience to LaTeX users.

%prep

%setup

%build
./configure --prefix=/usr
make all

%install
make install

%clean
make clean

%files
%dir /usr/lib/advi
/usr/bin/advi
/usr/lib/advi/splash.dvi
/usr/lib/advi/advilogo.eps
/usr/lib/advi/caml.eps
/usr/lib/advi/bar.jpg.eps
/usr/lib/advi/advi.sty
/usr/lib/advi/advi-annot.sty
/usr/lib/advi/advi-graphicx.sty
/usr/lib/advi/argv.sty
/usr/lib/advi/bubble.sty
/usr/lib/advi/superpose.sty
/usr/lib/advi/advi.pro
/usr/lib/advi/jpfonts.conf

%changelog

* Mon Feb 04 2002 <Jun.Furuse@inria.fr>

Packaged in a rpm. Here is the modus operandi:
cd /usr/src/redhat/SPECS/
cp advi.spec ./
rpm -ba --clean advi.spec
