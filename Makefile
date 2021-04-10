SUBDIRS = tex doc 

SRC = src/Makefile.config src/advi-latex-files src/main.exe

all: $(SRC)
	for dir in $(SUBDIRS); do make -C $$dir; done

.PHONY: advi
advi: src/main.exe

src/%: 
	dune build $@

.PHONY: test
test:
	make -C test

CONFIG = _build/src/Makefile.config

$(CONFIG): src/Makefile.config

.PHONY: doc doc.manual
doc: $(CONFIG)
	make -C doc

doc.manual: $(CONFIG)
	make -C doc manual

INSTALL = _build/default/advi.install

$(INSTALL):
	dune build @install

install: all $(INSTALL)
	dune install
	for dir in $(SUBDIRS); do make -C $$dir install; done
	@echo 
	@echo 'WARNING:'
	@echo "  You still need to run the command 'advi-latex-files'"
	@echo "  to install some LaTeX files needed for advanced features."

uninstall: $(INSTALL)
	for dir in $(SUBDIRS); do make -C $$dir uninstall; done
	dune uninstall

clean: 
	for dir in $(SUBDIRS) test; do make -C $$dir clean; done
	dune clean
