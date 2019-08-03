"""Format source files."""

from __future__ import print_function

import config
import subprocess

DEFAULT_FORMATTER = "clang-format"


def format(formatter=DEFAULT_FORMATTER):
    """Format all source files."""
    for src in config.ALL_SRCS:
        if subprocess.check_call([formatter, "-i", "--style=google", config.get_src(src)]):
            return False
        print("Formatted " + src)
    return True


def parse_args():
    """Parse formatting arguments."""
    from argparse import ArgumentParser
    from argparse import ArgumentDefaultsHelpFormatter
    parser = ArgumentParser(description="Python script for formatting qwip source files.",
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        "--formatter",
        default=DEFAULT_FORMATTER,
        help="The C++ formatter to use.")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    assert format(**vars(args))