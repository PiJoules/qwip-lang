#!/bin/bash
set -e

SRCS="src/Compiler.cpp src/Lexer.cpp src/Parser.cpp src/qwip.cpp"
HDRS="src/Compiler.h src/Diagnostics.h src/Lexer.h src/Parser.h"

ALL_SRC_FILES="$SRCS $HDRS"
