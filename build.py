"""Build qwip."""

from __future__ import print_function

import config
import os
import shlex
import subprocess

# NOTE: Due to an ABI incompatibility with projects built with Clang and LLVM
# libraries built with GCC, we will encounter a segfault in libLLVM
# (particularly during the createTargetMachine() step) if we build this project
# using Clang from LLVM libraries we get from apt-get.
# See https://groups.google.com/forum/#!topic/llvm-dev/8Tcz49kTNCw
#
# I have not yet tested this, but based off this, it should theoretically be
# possible to build everything using Clang if we build LLVM with Clang (or maybe
# at least locally). For at most LLVM 8 libraries, if we get them via apt-get,
# we should use GCC as the compiler.
DEFAULT_CPP = "g++-8"
DEFAULT_LLVM_CONFIG = "llvm-config-8"

CPPFLAGS = [
    "-O3", "-g", "-Werror", "-Wall", "-fno-exceptions", "-fno-rtti",
    "-std=c++14"
]
LINK_FLAGS = []


def build(**kwargs):
    """Build qwip."""
    cpp = kwargs["cpp"]
    llvm_config = kwargs["llvm_config"]
    build_asan = kwargs["build_asan"]
    extra_linker_args = kwargs["extra_linker_args"]
    working_dir = kwargs["change_dir"]

    if not os.path.exists(working_dir):
        os.makedirs(working_dir)

    llvm_cpp_flags = shlex.split(
        subprocess.check_output([llvm_config, "--cxxflags"]))
    llvm_cpp_flags.append("-Wno-unknown-warning-option")

    llvm_config_cmd = [
        llvm_config,
        "--ldflags",
        "--system-libs",
        "--libs",
    ]
    llvm_linker_flags = shlex.split(subprocess.check_output(llvm_config_cmd))

    # Make regular qwip executable
    srcs = [config.get_src(src) for src in config.SRCS]
    extra_linker_args = shlex.split(extra_linker_args)

    build_qwip_cmd = config.CPPCompilerCmdLineArgs(
        compiler=cpp,
        srcs=srcs,
        out=config.abs_join_path(working_dir, "qwip"),
        cppflags=llvm_cpp_flags + CPPFLAGS,
        linkflags=llvm_linker_flags + LINK_FLAGS + extra_linker_args)
    build_qwip = config.ProcessWrapper(
        descriptor="Building qwip executable", cmd=build_qwip_cmd.getcmd())

    # Make address sanitized qwip executable
    if build_asan:
        build_qwip_cmd.cppflags.append("-fsanitize=address")
        build_qwip_cmd.out = config.abs_join_path(working_dir, "qwip-asan")
        build_qwip_asan = config.ProcessWrapper(
            descriptor="Building address sanitized qwip executable",
            cmd=build_qwip_cmd.getcmd())

    if not build_qwip.passed():
        return False

    if build_asan and not build_qwip_asan.passed():
        return False

    return True


def parse_args():
    """Parse build arguments."""
    from argparse import ArgumentParser
    from argparse import ArgumentDefaultsHelpFormatter
    parser = ArgumentParser(
        description="Python script for building qwip.",
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        "--cpp", default=DEFAULT_CPP, help="C++ compiler to use.")
    parser.add_argument(
        "--llvm-config",
        default=DEFAULT_LLVM_CONFIG,
        help="Path to llvm-config to use.")
    parser.add_argument(
        "--build-asan",
        action="store_true",
        help="Build an executable that is also address sanitized.")
    parser.add_argument(
        "--extra-linker-args",
        type=str,
        default="",
        help="Extra arguments to pass when linking.")
    parser.add_argument(
        "-C",
        "--change-dir",
        default=config.WORKING_DIR,
        help="The working directory to change to when building.")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    assert build(**vars(args))
