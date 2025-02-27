#!/bin/bash -ex

cargo afl build

cargo afl fuzz -i ../pasko-testing/testsuite -o out target/debug/pasko-fuzzing
