SUBDIRS = src tex doc
# SUBDIRS = src 
DIST_SUBDIRS = $(SUBDIRS) examples

CVSRELEASETAG = Release-$(subst .,_,$(VERSION))
ANNOUNCEFILE  = Announce-$(VERSION)

WEBSITEDIR = /net/yquem/infosystems/www/cristal/cristal/$(PACKAGE)
FTPSITEDIR = /net/yquem/infosystems/ftp/$(PACKAGE)
CVSROOTDIR = /net/yquem/devel/caml/repository/
CVSPKGDIR  = bazar-ocaml/$(PACKAGE)

EXTRA_DIST = ocaml.m4 conf/jpfonts.conf LGPL

cvstag:
	cvs tag -c $(CVSRELEASETAG)

cvsuntag:
	cvs tag -d $(CVSRELEASETAG)

announce:
	mail -n -s "[ANN] New release $(VERSION) of $(NAME)" \
		advi@inria.fr \
		caml-announce@inria.fr \
	 < $(ANNOUNCEFILE)

FILEPERMS = -g cristal -m 644
DIRERMS = -g cristal -m 755
ROOTFILES = README INSTALL LICENSE
website: dist
	if [ -d $(WEBSITEDIR) ]; then     \
	: ; \
	else                              \
		$(INSTALL) -d $(DIRPERMS) $(WEBSITEDIR); \
		$(INSTALL) -d $(DIRPERMS) $(WEBSITEDIR)/archives/; \
	fi
	$(INSTALL) $(FILEPERMS) $(DIST_ARCHIVES) $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) $(DIST_ARCHIVES) $(WEBSITEDIR)/archive/
	$(INSTALL) $(FILEPERMS) $(ROOTFILES) $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.html $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/manual*.png $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.css $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.dvi $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.ps $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.pdf $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.gif $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/*.jpg $(WEBSITEDIR)
	$(INSTALL) $(FILEPERMS) doc/advi.1 $(WEBSITEDIR)

