"""Build qwip."""

from __future__ import print_function

import config
import shlex
import subprocess

DEFAULT_CPP = "clang++"
DEFAULT_LLVM_CONFIG = "llvm-config"

CPPFLAGS = [
    "-O3", "-g", "-Werror", "-Wall", "-fno-exceptions", "-fno-rtti",
    "-std=c++14"
]


def build(cpp=DEFAULT_CPP, llvm_config=DEFAULT_LLVM_CONFIG, build_asan=False):
    """Build qwip."""
    llvm_cpp_flags = shlex.split(
        subprocess.check_output([llvm_config, "--cxxflags"]))
    llvm_config_cmd = [
        llvm_config,
        "--ldflags",
        "--system-libs",
        "--libs",
    ]
    llvm_linker_flags = shlex.split(subprocess.check_output(llvm_config_cmd))

    # First compile
    processes = []
    asan_objs = []
    objs = []
    for src in config.SRCS:
        full_src = config.get_src(src)
        obj = full_src + ".o"
        cmd = [cpp, "-c", full_src, "-o", obj] + llvm_cpp_flags + CPPFLAGS
        proccess = config.ProcessWrapper(descriptor="Building " + obj, cmd=cmd)
        processes.append(proccess)
        objs.append(obj)
        print("Building", obj, "...")

        if build_asan:
            obj = full_src + "asan.o"
            cmd = [cpp, "-c", full_src, "-o", obj
                   ] + llvm_cpp_flags + CPPFLAGS + ["-fsanitize=address"]
            proccess = config.ProcessWrapper(
                descriptor="Building " + obj, cmd=cmd)
            processes.append(proccess)
            asan_objs.append(obj)
            print("Building", obj, "...")

    for proccess in processes:
        if not proccess.passed():
            return False

    # Now link
    link_qwip = config.ProcessWrapper(
        descriptor="Linking qwip",
        cmd=[cpp, "-o", "qwip"] + objs + llvm_linker_flags)
    if build_asan:
        link_qwip_asan = config.ProcessWrapper(
            descriptor="Linking qwip-asan",
            cmd=[cpp, "-o", "qwip-asan"] + objs + llvm_linker_flags +
            ["-fsanitize=address"])

    print("Linking qwip ...")
    if not link_qwip.passed():
        return False
    if build_asan:
        print("Linking qwip-asan ...")
        if not link_qwip_asan.passed():
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
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    assert build(**vars(args))
