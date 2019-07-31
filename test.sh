#!/bin/bash

source config.sh

declare -A PIDMAP

function add_test() {
  testname="$1"
  expected_out="$2"
  valgrind_tmpout="$TMPDIR/$testname.valgrind.out"
  valgrind_tmperr="$TMPDIR/$testname.valgrind.err"
  valgrind --leak-check=full --error-exitcode=1 ./qwip examples/$testname -o $testname.out > $valgrind_tmpout 2> $valgrind_tmperr && diff <(./$testname.out) <(printf "$expected_out") &
  pid="$!"
  PIDMAP[$pid]="valgrind tests/$testname;$valgrind_tmpout;$valgrind_tmperr"
}

add_test "1-empty-main.qw" ''
add_test "2-hello-world.qw" 'Hello World!\n'
add_test "3-variable-declaration.qw" 'Test\nTest\n'

for p in ${!PIDMAP[@]}; do
  vals="${PIDMAP[$p]}"
  testname="$(echo "$vals" | cut -d ';' -f 1)"
  tmpout="$(echo "$vals" | cut -d ';' -f 2)"
  tmperr="$(echo "$vals" | cut -d ';' -f 3)"
  if wait $p; then
    echo "PASS: $testname"
  else
    echo "FAIL: $testname"
    echo "See $tmpout for output and $tmperr for error logs"
    exit 1
  fi
done

echo "Everything seems to work!"
