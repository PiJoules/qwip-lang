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


def build_in_parallel(**kwargs):
    """Build tests in parallel."""
    use_valgrind = kwargs["use_valgrind"]
    use_asan = kwargs["use_asan"]
    working_dir = kwargs["change_dir"]

    print("Building tests...")
    qwip = config.abs_join_path(working_dir, "qwip")
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
        qwip_asan = config.abs_join_path(working_dir, "qwip-asan")
        if not qwip_asan:
            print("Could not find the sanitized qwip executable! Be sure to "
                  "build it first before running tests.")
            return False
        exe = [qwip_asan]

    processes = []
    for test in config.TESTS:
        out_file = config.abs_join_path(working_dir, test + ".out")
        test_stdout = config.get_tmp_file(test + ".stdout")
        test_stderr = config.get_tmp_file(test + ".stderr")
        cmd = config.QWIPCompilerCmdLineArgs(
            compiler=exe, srcs=[config.get_example(test)], out=out_file)
        process = config.ProcessWrapper(
            descriptor="Building " + test,
            cmd=cmd.getcmd(),
            stdout=test_stdout,
            stderr=test_stderr)
        processes.append(process)

    for process in processes:
        if not process.passed():
            print("FAIL:", process.descriptor)
            print("Error code {}. See {} and {} for debugging.".format(
                process.returncode(), process.stdout, process.stderr))
            return False

    return True


def run_in_parallel(**kwargs):
    """Run tests in parallel."""
    working_dir = kwargs["change_dir"]
    for test, expected in config.TESTS.iteritems():
        out_file = config.abs_join_path(working_dir, test + ".out")
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
    return run_in_parallel(**kwargs)


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
    parser.add_argument(
        "-C",
        "--change-dir",
        default=config.WORKING_DIR,
        help="The working directory to change to when testing.")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    assert run_tests(**vars(args))
