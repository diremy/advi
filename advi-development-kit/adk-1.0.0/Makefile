install:
	(cd camltk418; ./configure --with-config=site.config; \
		make all opt install installopt)
	(cd camlimages-2.00; ./configure; \
		make all opt install) 
	(cd advi-1.0.0; ./configure; make all install) 

clean:
	(cd camltk418; make clean)
	(cd camlimages-2.00; make clean)
	(cd advi-1.0.0; make clean)
