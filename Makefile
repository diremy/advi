SUBDIRS = tex doc 

SRC = src/Makefile.config src/advi-latex-sources src/main.exe

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
	echo "This installed the 'advi' command. "
	echo "For advanced features of advi you also need to install latex-sources."
	echo "To do thus, run the command 'advi-latex-sources'."

uninstall:
	for dir in $(SUBDIRS); do make -C $$dir uninstall; done
	dune uninstall

clean: 
	for dir in $(SUBDIRS) test; do make -C $$dir clean; done
	dune clean
