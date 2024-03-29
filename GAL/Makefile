# "make test" Compiles everything and runs the regression tests

.PHONY: test
test: all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY: all
all: gal.native GAL.o

# "make gal.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

gal.native: GAL.bc
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader  -cflags -w,+a-3 gal.native


GAL.bc: GAL.c
	clang -emit-llvm -o GAL.bc -c GAL.c -Wno-varargs

# "make clean" removes all generated files

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff gal 
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.err *.exe *~
	rm -rf *.o
	rm -rf *.bc
	rm -rf *.output


# Testing the "printbig" example
GAL: GAL.c
	cc -o GAL -DBUILD_TEST GAL.c

# Building the tarball

TESTS = \
  add1 arith1 arith2 arith3 fib float1 float2 float3 for1 for2 func1 \
  func2 func3 func4 func5 func6 func7 func8 func9 gcd2 gcd global1 \
  global2 global3 hello if1 if2 if3 if4 if5 if6 local1 local2 ops1 \
  ops2 printbig var1 var2 while1 while2

FAILS = \
  assign1 assign2 assign3 dead1 dead2 expr1 expr2 expr3 float1 float2 \
  for1 for2 for3 for4 for5 func1 func2 func3 func4 func5 func6 func7 \
  func8 func9 global1 global2 if1 if2 if3 nomain printbig printb print \
  return1 return2 while1 while2

TESTFILES = $(TESTS:%=test-%.gal) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.gal) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags gal.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	GAL.c GAL.h arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=gal-tests/%) 

gal.tar.gz : $(TARFILES)
	cd .. && tar czf gal.tar.gz \
		$(TARFILES:%=GAL/%)
