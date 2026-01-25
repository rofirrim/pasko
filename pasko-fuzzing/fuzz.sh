#!/bin/bash -ex

cargo afl build --release

cargo afl fuzz -i ../pasko-testing/testsuite -o out target/release/pasko-fuzzing
