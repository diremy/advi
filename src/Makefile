FILES = Makefile.config advi-latex-files main.exe

DUNEROOT = --root=..

all: $(FILES)

$(FILES):
	dune build $(DUNEROOT) $@

install:
	dune build $(DUNEROOT) @install
	dune install $(DUNEROOT)
