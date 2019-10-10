# qwip

master branch status:

[![Build Status](https://travis-ci.org/PiJoules/qwip-lang.svg?branch=master)](https://travis-ci.org/PiJoules/qwip-lang)

dev branch status:

[![Build Status](https://travis-ci.org/PiJoules/qwip-lang.svg?branch=dev)](https://travis-ci.org/PiJoules/qwip-lang)

## Building and Running

### Requirements

```
# This project is currently only tested with g++-8 and llvm-8.
# Other compilers and LLVM versions should theoretically work, but have not been
# tested.
g++-8         # The chosen compiler for building this project
llvm-8        # The backend for this language
cmake v3.4.3  # For building the project
python 2.7    # For running a script used for testing
valgrind      # (Optional) For testing
```

### Build steps

```sh
# Only needs to be done once
$ sudo apt-get -y g++-8
$ sudo apt-get install -y libllvm-8-ocaml-dev libllvm8 llvm-8 llvm-8-dev llvm-8-doc llvm-8-examples llvm-8-runtime lld-8
$ sudo apt-get install -y valgrind  # Optional
$ git clone https://github.com/PiJoules/qwip-lang.git
$ mkdir qwip-build
$ cd qwip-build

# Only needs to be once you have the pre-requisites and a build directory.
$ cmake -G Ninja -DLLVM_DIR=/usr/lib/llvm-8/lib/cmake/llvm -DCMAKE_CXX_COMPILER=g++-8 ../qwip-lang  # The LLVM_DIR can be replaced with wherever the `LLVMConfig.cmake` file is located. This is just where apt-get places the library on installation.
$ ninja qwip
$ ./qwip ../qwip-lang/examples/2-hello-world.qw -o hello_world.out
$ ./hello_world.out
Hello World!
```

### Testing

You can run tests with

```sh
$ ninja test
```

Alternatively, you can change the tests to run against valgrind.

```sh
$ cmake -G Ninja -DLLVM_DIR=/usr/lib/llvm-8/lib/cmake/llvm -DCMAKE_CXX_COMPILER=g++-8 -DTEST_WITH=VALGRIND ../qwip-lang
$ ninja qwip
$ ninja test
```

Or with ASan

```sh
$ cmake -G Ninja -DLLVM_DIR=/usr/lib/llvm-8/lib/cmake/llvm -DCMAKE_CXX_COMPILER=g++-8 -DTEST_WITH=ASAN ../qwip-lang
$ ninja qwip-asan
$ ninja test
```

## Misc Notes

- I am able to build this on my mac, although I kept getting a segfault when
  building with ASan on mac. I'm more willing to believe this is just my mac's
  ASan that's the issue rather than the code since ASan and valgrind on linux
  don't raise any issues.
