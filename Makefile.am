SUBDIRS = src doc tex

CVSRELEASETAG = Release-$(subst .,_,$(VERSION))
ANNOUNCEFILE  = Announce-$(VERSION)

WEBSITEDIR = /net/yquem/infosystems/www/$(PACKAGE)
FTPSITEDIR = /net/yquem/infosystems/ftp/$(PACKAGE)
CVSROOTDIR = /net/yquem/devel/caml/repository/
CVSPKGDIR  = bazar-ocaml/$(PACKAGE)

cvstag:
	cvs tag -c $(CVSRELEASETAG)

cvsuntag:
	cvs tag -d $(CVSRELEASETAG)

announce:
	mail -n -s "New release $(VERSION) of $(NAME)" \
		caml-announce@inria.fr < $(ANNOUNCEFILE)

website:
	if [ -d $(WEBSITEDIR) ]; then     \
		$(RM) -f $(WEBSITEDIR) *; \
	else                              \
		$(INSTALL) -d -g caml -m g+w $(WEBSITEDIR);
	done
	$(INSTALL) -g caml -m g+w doc/*.htm $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.css $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.dvi $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.ps $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.pdf $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.gif $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.jpg $(WEBSITEDIR)
	$(INSTALL) -g caml -m g+w doc/*.$(MANEXT) $(WEBSITEDIR)
	cd $(WEBSITEDIR) && ln -sf eng.htm index.html

ftpsite: dist
	if [ ! -d $(FTPSITEDIR) ]; then                     \
		$(INSTALL) -d -g caml -m g+w $(FTPSITEDIR); \
	done
	$(INSTALL) -g caml -m g+w $(DISTFILE) $(FTPSITEDIR)
