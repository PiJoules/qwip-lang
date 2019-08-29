"""Run Tests."""

from __future__ import print_function

import config
import distutils.spawn
import os.path
import subprocess


def find_exe(exe):
    """Get the absolute path to an executable."""
    exe = distutils.spawn.find_executable(exe)
    if not exe:
        return None
    return os.path.abspath(exe)


def build_in_parallel(use_valgrind=False, use_asan=False):
    """Build tests in parallel."""
    print("Building tests...")
    qwip = find_exe("qwip")
    if not qwip:
        print("Could not find the qwip executable! Be sure to build it first "
              "before running tests.")
        return False
    exe = [qwip]

    if use_valgrind:
        valgrind_path = find_exe("valgrind")
        if valgrind_path:
            exe = [valgrind_path, "--leak-check=full", "--error-exitcode=1"
                   ] + exe
        else:
            print("Warning: Unable to find valgrind. Continuing to build "
                  "without it.")
    elif use_asan:
        qwip_asan = find_exe("qwip-asan")
        if not qwip_asan:
            print("Could not find the sanitized qwip executable! Be sure to "
                  "build it first before running tests.")
            return False
        exe = [qwip_asan]

    processes = []
    for test in config.TESTS:
        test_file = config.get_example(test)
        out_file = test_file + ".out"
        test_stdout = config.get_tmp_file(test + ".stdout")
        test_stderr = config.get_tmp_file(test + ".stderr")
        cmd = exe + [
            test_file,
            "-o",
            out_file,
        ]
        process = config.ProcessWrapper(
            descriptor="Building " + test,
            cmd=cmd,
            stdout=test_stdout,
            stderr=test_stderr)
        processes.append(process)

    for process in processes:
        if process.passed():
            print("PASS:", process.descriptor)
        else:
            print("FAIL:", process.descriptor)
            print("Error code {}. See {} and {} for debugging.".format(
                process.returncode(), process.stdout, process.stderr))
            return False

    return True


def run_in_parallel():
    """Run tests in parallel."""
    for test, expected in config.TESTS.iteritems():
        out_file = config.get_example(test) + ".out"
        output = subprocess.check_output(out_file)
        if output == expected:
            print("PASS: Checking", test)
        else:
            print("FAIL: Checking", test)
            print("Expected:", expected)
            print("Found:", output)
            return False
    return True


def run_tests(**kwargs):
    """Build and run tests."""
    if not build_in_parallel(**kwargs):
      return False
    print("--------------")
    return run_in_parallel()


def parse_args():
    """Parse arguments."""
    from argparse import ArgumentParser
    from argparse import ArgumentDefaultsHelpFormatter
    parser = ArgumentParser(
        description="Python script for testing qwip.",
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        "--use-valgrind",
        action="store_true",
        help="Use valgring when running tests.")
    parser.add_argument(
        "--use-asan",
        action="store_true",
        help="Use the address sanitized version of qwip when running tests.")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    assert run_tests(**vars(args))
