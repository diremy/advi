Release: 1
Version: 1.0.0
Name: advi
Summary: Active-DVI Presenter
Source: ftp://ftp.inria.fr/INRIA/Projects/cristal/advi/advi-1.0.0.tgz
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
./configure
make all

%install
make install

%clean
make clean

%files
%dir /usr/local/lib/advi
/usr/local/bin/advi
/usr/local/lib/advi/splash.dvi
/usr/local/lib/advi/advilogo.eps
/usr/local/lib/advi/caml.eps
/usr/local/lib/advi/bar.jpg.eps
/usr/local/lib/advi/advi.sty
/usr/local/lib/advi/advi-annot.sty
/usr/local/lib/advi/advi-graphicx.sty
/usr/local/lib/advi/argv.sty
/usr/local/lib/advi/bubble.sty
/usr/local/lib/advi/superpose.sty
/usr/local/lib/advi/advi.pro
/usr/local/lib/advi/jpfonts.conf

%changelog

* Mon Feb 04 2002 <Jun.Furuse@inria.fr>

Packaged in a rpm. Here is the modus operandi:
cd /usr/src/RPM/SPECS/
cp advi.spec ./
rpm -bb --clean advi.spec
