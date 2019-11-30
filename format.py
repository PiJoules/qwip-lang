"""Format source files. Run from the root project directory."""

from __future__ import print_function

import os
import os.path
import subprocess

DEFAULT_FORMATTER = "clang-format"


def format(formatter=DEFAULT_FORMATTER):
  """Format all source files."""
  for root, _, files in os.walk("."):
    for name in files:
      _, ext = os.path.splitext(name)
      if not (ext == ".h" or ext == ".cpp"):
        continue
      path = os.path.join(root, name)
      if subprocess.check_call([formatter, "-i", "--style=google", path]):
        return False
      print("Formatted " + path)
  return True


def parse_args():
  """Parse formatting arguments."""
  from argparse import ArgumentParser
  from argparse import ArgumentDefaultsHelpFormatter
  parser = ArgumentParser(
      description="Python script for formatting qwip source files.",
      formatter_class=ArgumentDefaultsHelpFormatter)
  parser.add_argument("--formatter",
                      default=DEFAULT_FORMATTER,
                      help="The C++ formatter to use.")
  return parser.parse_args()


if __name__ == "__main__":
  args = parse_args()
  assert format(**vars(args))
