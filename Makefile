SUBDIRS = tex doc test

DIRS = src $(SUBDIRS)

SRC = src/Makefile.config src/advi-install src/main.exe

all: $(SRC)
	for dir in $(SUBDIRS); do make -C $$dir; done

.PHONY: advi
advi: src/main.exe

src/%: 
	dune build $@

install: all
	dune build @install
	dune install
	for dir in $(SUBDIRS); do make -C $$dir install; done

clean: 
	for dir in $(SUBDIRS); do make -C $$dir clean; done
	dune clean
