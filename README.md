# qwip

```sh
$ python build.py
$ python test.py
$ ./qwip examples/2-hello-world.qw -o hello_world.out
$ ./hello_world.out
Hello World!
```

## Requirements

```
python 2.7  # For building and testing
llvm v8.0.0
bash 4
valgrind  # (Optional) For testing
```

## Installing LLVM on Mac

```
$ brew install llvm
```

Then see https://stackoverflow.com/a/52530212/2775471 if clang doesn't work.

## Installing valgrind on Mac OS Mojave

https://stackoverflow.com/a/56831383/2775471