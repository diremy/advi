# make a big tar ball
FTPSITEDIR=/net/pauillac/infosystems/ftp/cristal

distribute: clean
	(cd adk-1.0.0; \
	   tar zxvf $(FTPSITEDIR)/caml-light/bazar-ocaml/ocamltk/ocamltk418.tar.gz; \
	   tar zxvf $(FTPSITEDIR)/caml-light/bazar-ocaml/camlimages-2.00.tgz; \
	   tar zxvf $(FTPSITEDIR)/advi/advi-1.0.0.tar.gz)
	tar zcvf adk-1.0.0.tar.gz adk-1.0.0

clean:
	(cd adk-1.0.0; \
	   /bin/rm -rf camltk* camlimages* advi*)
