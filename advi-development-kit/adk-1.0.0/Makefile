CAMLTK=camltk418
CAMLIMAGES=camlimages-2.00
ADVI=advi-1.0.0

install:
	(cd $(CAMLTK); \
	 if test ! -f Makefile.config; then \
	   ./configure --with-config=site.config; \
	 fi)
	(cd $(CAMLTK); make all opt install installopt)

	(cd $(CAMLIMAGES); \
	 if test ! -f Makefile.config; then \
	   ./configure; \
	 fi)
	(cd $(CAMLIMAGES); make all opt install) 

	(cd $(ADVI); \
	 if test ! -f Makefile.config; then \
	   ./configure; \
	 fi)
	(cd $(ADVI); make all install) 

reconfigure: clean
	rm -f $(CAMLTK)/Makefile.config
	rm -f $(CAMLIMAGES)/Makefile.config
	rm -f $(ADVI)/Makefile.config
	make install

clean:
	(cd $(CAMLTK); make clean)
	(cd $(CAMLIMAGES); make clean)
	(cd $(ADVI); make clean)
