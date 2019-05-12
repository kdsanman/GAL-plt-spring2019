# plt-spring2019

To run:
```
make
./microc.native -a  (ast check)
./microc.native -s test-hello.mc  
./microc.native test-hello.mc > temp.ll
llc temp.ll
clang GAL.c temp.s
./a.out
 ```
