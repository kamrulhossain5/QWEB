# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable
# to test linking external code

.PHONY : all
all : qweb.native

# "make qweb.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

qweb.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind qweb.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff

# Building the tarball

TESTS = \
  hello

TESTFILES = $(TESTS:%=test-%.qwb) $(TESTS:%=test-%.out)

TARFILES = ast.ml Makefile _tags qweb.ml parser.mly \
	scanner.mll testall.sh \
	$(TESTFILES:%=tests/%) 
