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
	mail -n -s "New release $(VERSION) of $(NAME)" \
		advi@inria.fr \
		caml-announce@inria.fr \
	 < $(ANNOUNCEFILE)

PERMS = -g cristal -m 755
WEBFILES = README INSTALL LICENSE
website: dist
	if [ -d $(WEBSITEDIR) ]; then     \
	: ; \
	else                              \
		$(INSTALL) -d -g caml -m 775 $(WEBSITEDIR); \
	fi
	$(INSTALL) $(PERMS) $(DIST_ARCHIVES) $(WEBSITEDIR)
	$(INSTALL) $(PERMS) $(WEBFILES) $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.html $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.css $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.dvi $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.ps $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.pdf $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.gif $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/*.jpg $(WEBSITEDIR)
	$(INSTALL) $(PERMS) doc/advi.1 $(WEBSITEDIR)
	cd $(WEBSITEDIR) && ln -sf eng.html index.html

ftpsite: dist
	if [ ! -d $(FTPSITEDIR) ]; then                     \
		$(INSTALL) -d -g caml -m 775 $(FTPSITEDIR); \
	fi
	$(INSTALL) $(PERMS) $(DIST_ARCHIVES) $(FTPSITEDIR)
