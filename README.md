# GAL: Graph Algortithm Language

This language compiler was created for the Programming Languages and Translators class.
Test files you could use to test the language in GAL folder:
- test-gal.gal

Additional test files to look at are in gal-tests/ directory.

## How to compile, run and test
To run make sure you are inside the repository GAL:
```
git clone https://github.com/kdsanman/plt-spring2019.git
cd plt-spring2019
make
./gal.native -a TEST_FILE_NAME
./gal.native -s TEST_FILE_NAME  
./gal.native TEST_FILE_NAME > temp.ll
llc temp.ll
clang GAL.c temp.s
./a.out
 ```
 ## Example
 ```
make clean ; make
./gal.native -a test-gal.gal
./gal.native -s test-gal.gal  
./gal.native test-gal.gal > temp.ll
llc temp.ll
clang GAL.c temp.s
./a.out
 ```
