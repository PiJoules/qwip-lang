# qwip

```sh
$ python build.py
$ python test.py
$ ./out/qwip examples/2-hello-world.qw -o hello_world.out
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

## Misc Notes

- I am able to build this on my mac, although I kept getting a segfault when
  building with ASan on mac. I'm more willing to believe this is just my mac's
  ASan that's the issue rather than the code since ASan and valgrind on linux
  don't raise any issues.
