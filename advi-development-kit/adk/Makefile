CAMLIMAGES=camlimages-2.11
ADVI=advi-1.4.0

install:
	(cd $(CAMLIMAGES); \
	 if test ! -f Makefile.config; then \
	   ./configure; \
	 fi)
	(cd $(CAMLIMAGES); $(MAKE) all opt install) 

	(cd $(ADVI); \
	 if test ! -f Makefile.config; then \
	   ./configure; \
	 fi)
	(cd $(ADVI); $(MAKE) all install) 

reconfigure: clean
	rm -f $(CAMLTK)/Makefile.config
	rm -f $(CAMLIMAGES)/Makefile.config
	rm -f $(ADVI)/Makefile.config
	$(MAKE) install

clean:
	(cd $(CAMLTK); $(MAKE) clean)
	(cd $(CAMLIMAGES); $(MAKE) clean)
	(cd $(ADVI); $(MAKE) clean)
