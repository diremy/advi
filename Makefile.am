SUBDIRS = src tex doc examples

CVSRELEASETAG = Release-$(subst .,_,$(VERSION))
ANNOUNCEFILE  = Announce-$(VERSION)

WEBSITEDIR = /net/yquem/infosystems/www/cristal/cristal/$(PACKAGE)
FTPSITEDIR = /net/yquem/infosystems/ftp/$(PACKAGE)
CVSROOTDIR = /net/yquem/devel/caml/repository/
CVSPKGDIR  = bazar-ocaml/$(PACKAGE)

EXTRA_DIST = ocaml.m4

cvstag:
	cvs tag -c $(CVSRELEASETAG)

cvsuntag:
	cvs tag -d $(CVSRELEASETAG)

announce:
	mail -n -s "New release $(VERSION) of $(NAME)" \
		caml-announce@inria.fr < $(ANNOUNCEFILE)

website: dist
	if [ -d $(WEBSITEDIR) ]; then     \
	: ; \
	else                              \
		$(INSTALL) -d -g caml -m 775 $(WEBSITEDIR); \
	fi
	$(INSTALL) -g caml -m 755 $(DIST_ARCHIVES) $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.html $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.css $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.dvi $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.ps $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.pdf $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.gif $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/*.jpg $(WEBSITEDIR)
	$(INSTALL) -g caml -m 755 doc/advi.1 $(WEBSITEDIR)
	cd $(WEBSITEDIR) && ln -sf eng.html index.html

ftpsite: dist
	if [ ! -d $(FTPSITEDIR) ]; then                     \
		$(INSTALL) -d -g caml -m 775 $(FTPSITEDIR); \
	fi
	$(INSTALL) -g caml -m 755 $(DIST_ARCHIVES) $(FTPSITEDIR)
