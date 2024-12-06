#llvm code, output in the terminal
# dune exec -- ./bin/toplevel.exe -l ./test/test.mj
dune exec -- ./src/toplevel.exe -a "./test/$1" > "test.ast" 
dune exec -- ./src/toplevel.exe -s "./test/$1" > "test.sast" 
dune exec -- ./src/toplevel.exe -l "./test/$1" > "test.ll" 
llc -relocation-model=pic "test.ll" > "test.s"
cc -o "test.exe" "test.s"
"./test.exe" 

#> "test.out" 
# if diff -q "test.out" "test.ref" >/dev/null ; then
#     echo "test PASSED"
# else
#     echo "test FAILED"
# fi