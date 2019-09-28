"""Configuration for this repo."""

from __future__ import print_function

import os.path
import subprocess
import tempfile

TEST_DIR = "examples"
SRCS_DIR = "src"
TMP_DIR = tempfile.gettempdir()
WORKING_DIR = "out"

# Add new sources and headers here!
SRCS = [
    "Compiler.cpp",
    "Lexer.cpp",
    "Parser.cpp",
    "qwip.cpp",
]
HDRS = [
    "Compiler.h",
    "Diagnostics.h",
    "Lexer.h",
    "Parser.h",
]
ALL_SRCS = SRCS + HDRS

# Add new tests here!
TESTS = {
    "1-empty-main.qw": "",
    "2-hello-world.qw": "Hello World!\n",
    "3-variable-declaration.qw": "Test\nTest\n",
    "4-print-format.qw": "10\n11\n",
    "5-fibonacci.qw": "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n",
    "6-structures.qw": "7\n3\n",
}


def abs_join_path(dirname, filename):
    """Short function for getting the absolute path after joining."""
    return os.path.abspath(os.path.join(dirname, filename))


def get_example(example):
    """Get the full path to an example under the test directory."""
    return abs_join_path(TEST_DIR, example)


def get_src(src):
    """Get the full path to a source file under the source file directory."""
    return abs_join_path(SRCS_DIR, src)


def get_tmp_file(name):
    """Return a named temporary file under the temporary directory."""
    return abs_join_path(TMP_DIR, name)


def init_kwargs(self, kwargs):
    """Iniialize the slots of a class."""
    for attr in self.__public_slots__:
        if attr in kwargs:
            setattr(self, attr, kwargs[attr])
        else:
            setattr(self, attr, self.__defaults__[attr])


class ProcessWrapper(object):
    """Ease of use class for holding metadata info on a process running."""

    __public_slots__ = ("cmd", "stdout", "stderr", "descriptor")
    __defaults__ = {
        "descriptor": None,
        "stdout": None,
        "stderr": None,
    }
    __slots__ = __public_slots__ + ("process",)

    def __init__(self, **kwargs):
        """Initialize properties."""
        init_kwargs(self, kwargs)

        stdout = self.stdout
        if stdout:
            stdout = open(stdout, "w")
        stderr = self.stderr
        if stderr:
            stderr = open(stderr, "w")

        self.process = subprocess.Popen(self.cmd, stdout=stdout, stderr=stderr)
        print(self.descriptor, "...")

    def passed(self):
        """Check if this process passed."""
        return self.returncode() == 0

    def returncode(self):
        """Get the return code of this process."""
        self.process.wait()
        return self.process.returncode


class CPPCompilerCmdLineArgs:
    """Ease of use class for handling arguments to a C++ compiler."""

    __public_slots__ = ("compiler", "srcs", "cppflags", "linkflags", "out")
    __defaults__ = {
        "cppflags": [],
        "linkflags": [],
    }
    __slots__ = __public_slots__

    def __init__(self, **kwargs):
        """Initialize properties."""
        init_kwargs(self, kwargs)

        assert isinstance(self.compiler, str)
        assert isinstance(self.srcs, list)
        assert isinstance(self.cppflags, list)
        assert isinstance(self.linkflags, list)
        assert isinstance(self.out, str)

    def getcmd(self):
        """Return the whole command line invocation as a list of strings."""
        return [self.compiler] + self.srcs + self.cppflags + self.linkflags + [
            "-o", self.out
        ]


class QWIPCompilerCmdLineArgs:
    """Ease of use class for handling arguments to the qwip compiler."""

    __public_slots__ = ("compiler", "srcs", "out")
    __slots__ = __public_slots__

    def __init__(self, **kwargs):
        init_kwargs(self, kwargs)

    def getcmd(self):
        """Return the whole command line invocation as a list of strings."""
        if isinstance(self.compiler, list):
            # This covers the case where we want to use valgrind to wrap around
            # our compile invocation.
            return self.compiler + self.srcs + ["-o", self.out]
        return [self.compiler] + self.srcs + ["-o", self.out]
