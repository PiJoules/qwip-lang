#!/bin/bash

source config.sh

CC="clang++-5.0"
CPPFLAGS="-O3 -g -Werror -Wall -fno-exceptions -fno-rtti -std=c++14"
LLVM_CONFIG=llvm-config-5.0

$CC -Wno-unknown-warning-option -Wno-unused-command-line-argument $($LLVM_CONFIG --cxxflags --ldflags --system-libs --libs core) $CPPFLAGS $SRCS -o "qwip"
