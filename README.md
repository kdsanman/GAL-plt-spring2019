# plt-spring2019

To run:
```
make
./microc.native -a TEST_FILE_NAME
./microc.native -s TEST_FILE_NAME  
./microc.native TEST_FILE_NAME > temp.ll
llc temp.ll
clang GAL.c temp.s
./a.out
 ```
