FILES = Makefile.config advi-latex-files main.exe

all: $(FILES)

$(FILES):
	dune build $@

install:
	dune build @install
	dune install
