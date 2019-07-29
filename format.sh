#!/bin/bash
source config.sh

for src in $ALL_SRC_FILES; do
  clang-format -i --style=google $src
  echo "Formatted $src"
done
