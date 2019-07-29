#!/bin/bash

source config.sh

valgrind --leak-check=full --error-exitcode=1 ./qwip examples/1-empty-main.cpp
