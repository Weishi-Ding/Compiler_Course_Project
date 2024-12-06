#!/bin/bash

# If a filename is passed as an argument, test only that file.
# Otherwise, test all files.
if [ $# -eq 1 ]; then
    files=("$1")
else
    files=("FailTest1" "FailTest2" "FailTest3" "FailTest4" 
           "Test1" "Test2" "Test3" "Test4" "Test5" "Test6" "Test7" "Test8" "Test9" "Test10"
           "Test11" "Test12" "Test13" "Test14" "Test15" "Test16" "Test17" "Test18" "Test19" "Test20")
fi

# Test each file in the array.
for f in "${files[@]}"; do
    if [[ "$f" == FailTest* ]]; then
        expected_output="./test/$f.out"
        actual_output=$(dune exec -- ./src/toplevel.exe -l "./test/$f" 2>&1 )
        if [[ "$actual_output" != "$(cat $expected_output)" ]]; then
            echo "Failed Test failed as expected: $f"
        else
            echo "Failed Test fail as expected: $f"
        fi
    elif [[ "$f" == Test* ]]; then
        expected_output="./test/$f.out"
        dune exec -- ./src/toplevel.exe -l "./test/$f" > "test.ll"
        llc -relocation-model=pic "test.ll" > "test.s"
        cc -o "test.exe" "test.s"
        actual_output=$(./test.exe)
        if [[ "$actual_output" != "$(cat $expected_output)" ]]; then
            echo "Test failed: $f"
        else
            echo "Test passed: $f"
        fi
    fi
done
