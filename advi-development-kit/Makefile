# make a big tar ball
FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal

distribute: clean
	(cd adk; \
	   tar zxvf $(FTPSITEDIR)/caml-light/bazar-ocaml/camlimages-2.11.tgz; \
	   tar zxvf $(FTPSITEDIR)/advi/advi-1.4.0.tar.gz)
	tar zcvf adk-1.4.0.tar.gz --exclude '*~' --exclude .cvsignore --exclude CVS adk
	cp adk-1.4.0.tar.gz $(FTPSITEDIR)/advi

clean:
	rm -f adk-1.4.0.tar.gz
	(cd adk; \
	   /bin/rm -rf camlimages* advi*)
