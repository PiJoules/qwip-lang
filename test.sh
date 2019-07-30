#!/bin/bash

source config.sh

valgrind --leak-check=full --error-exitcode=1 ./qwip examples/1-empty-main.qw -o 1-empty-main.out
valgrind --leak-check=full --error-exitcode=1 ./1-empty-main.out
diff <(./1-empty-main.out) <(printf "")

valgrind --leak-check=full --error-exitcode=1 ./qwip examples/2-hello-world.qw -o 2-hello-world.out
valgrind --leak-check=full --error-exitcode=1 ./2-hello-world.out
diff <(./2-hello-world.out) <(printf "Hello World!\n")

echo "Everything seems to work!"
