# make a big tar ball
ADKVERSION=1.4.0
CAMLIMAGESVERSION=2.11

FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal

distribute: clean
	(cd adk; \
	   tar zxvf $(FTPSITEDIR)/caml-light/bazar-ocaml/camlimages-$(CAMLIMAGESVERSION).tgz; \
	   tar zxvf $(FTPSITEDIR)/advi/advi-$(ADKVERSION).tar.gz)
	tar zcvf adk-$(ADKVERSION).tar.gz --exclude '*~' --exclude .cvsignore --exclude CVS adk
	cp adk-$(ADKVERSION).tar.gz $(FTPSITEDIR)/advi

clean:
	rm -f adk-$(ADKVERSION).tar.gz
	(cd adk; \
	   /bin/rm -rf camlimages* advi*)
