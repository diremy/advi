FILES = Makefile.config advi-latex-files main.exe

DUNE = dune --root=..

all: $(FILES)

$(FILES):
	$(DUNE) build $@

install:
	$(DUNE) build @install
	$(DUNE) install
