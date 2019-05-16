# plt-spring2019

Test files you could use on GAL folder:
- test-hello.gal
- test-list.gal
- test-graph.gal

Additional test files to look at are in gal-tests/ directory.

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
