# plt-spring2019

To run make sure you are inside the repository GAL:
```
make
./gal.native -a TEST_FILE_NAME
./gal.native -s TEST_FILE_NAME  
./gal.native TEST_FILE_NAME > temp.ll
llc temp.ll
clang GAL.c temp.s
./a.out
 ```
