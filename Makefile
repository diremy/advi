SUBDIRS = tex doc 

SRC = src/Makefile.config src/advi-latex-files src/main.exe

all: $(SRC)
	for dir in $(SUBDIRS); do make -C $$dir; done

.PHONY: advi
advi: src/main.exe

src/%: 
	dune build $@

.PHONE: test
test:
	make -C test

install: all
	dune build @install
	dune install
	for dir in $(SUBDIRS); do make -C $$dir install; done
	@echo 
	@echo 'WARNING:'
	@echo "  You still need to run the command 'advi-latex-files'"
	@echo "  to install some LaTeX files needed for advanced features."

uninstall:
	for dir in $(SUBDIRS); do make -C $$dir uninstall; done
	dune uninstall

clean: 
	for dir in $(SUBDIRS) test; do make -C $$dir clean; done
	dune clean
