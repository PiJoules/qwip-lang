"""Collect code coverage information. Run from the project BUILD directory."""

from __future__ import print_function

import os
import os.path
import subprocess

DEFAULT_LCOV_TOOL = "lcov"
DEFAULT_GCOV_TOOL = "gcov"


def main(lcov_tool=DEFAULT_LCOV_TOOL, gcov_tool=DEFAULT_GCOV_TOOL):
    base_lcov_cmd = [lcov_tool, "--gcov-tool", gcov_tool]

    # Generate the coverage.info file.
    if subprocess.check_call(
            base_lcov_cmd +
        ["--capture", "--directory", ".", "--output-file", "coverage.info"]):
        return False

    # Retain coverage for only the files in this project.
    if subprocess.check_call(base_lcov_cmd + [
            "-e", "coverage.info", "--output-file", "coverage.info",
            "*qwip-lang/src/*"
    ]):
        return False

    # Print out coverage info.
    if subprocess.check_call(base_lcov_cmd + ["--list", "coverage.info"]):
        return False

    # Run gcov on all generated .gcda files to get info for source files.
    for root, _, files in os.walk("CMakeFiles/qwip.dir/"):
        for name in files:
            _, ext = os.path.splitext(name)
            if ext != ".gcda":
                continue
            path = os.path.join(root, name)
            with open(os.devnull, "w") as null:
                if subprocess.check_call([gcov_tool, path], stdout=null):
                    return False
            print("Generated gcov file for", path)

    return True


def parse_args():
    """Parse coverage arguments."""
    from argparse import ArgumentParser
    from argparse import ArgumentDefaultsHelpFormatter
    parser = ArgumentParser(
        description="Python script for collecting coverage for qwip.",
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument("--lcov-tool",
                        default=DEFAULT_LCOV_TOOL,
                        help="The lcov tool to use.")
    parser.add_argument("--gcov-tool",
                        default=DEFAULT_GCOV_TOOL,
                        help="The gcov tool to use.")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    assert main(**vars(args))
