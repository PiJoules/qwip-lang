"""Collect code coverage information. Run from the project BUILD directory."""

from __future__ import print_function

import distutils.spawn
import os
import os.path
import subprocess

DEFAULT_LCOV_TOOL = "lcov"
DEFAULT_GCOV_TOOL = "gcov"


def main(lcov_tool=DEFAULT_LCOV_TOOL, gcov_tool=DEFAULT_GCOV_TOOL):
  if not distutils.spawn.find_executable(lcov_tool):
    print("Could not find", lcov_tool)
    return False
  if not distutils.spawn.find_executable(gcov_tool):
    print("Could not find", gcov_tool)
    return False

  base_lcov_cmd = [lcov_tool, "--gcov-tool", gcov_tool]

  # Generate the coverage.info file.
  if subprocess.check_call(
      base_lcov_cmd +
      ["--capture", "--directory", ".", "--output-file", "coverage.info"]):
    return False

  # Retain coverage for only the files in this project.
  if subprocess.check_call(base_lcov_cmd + [
      "-e",
      "coverage.info",
      "--output-file",
      "coverage.info",
      "*qwip-lang/src/*",
  ]):
    return False

  # Remove external sources.
  if subprocess.check_call(base_lcov_cmd + [
      "--remove",
      "coverage.info",
      "*.def",
      "--output-file",
      "coverage.info",
      "*qwip-lang/src/catch.hpp",
  ]):
    return False

  # Print out coverage info.
  if subprocess.check_call(base_lcov_cmd + ["--list", "coverage.info"]):
    return False

  # Run gcov on all generated .gcda files to get info for source files.
  gc_files = []
  for root, _, files in os.walk("CMakeFiles/"):
    for name in files:
      _, ext = os.path.splitext(name)
      if not (ext == ".gcda" or ext == ".gcno"):
        continue
      path = os.path.join(root, name)
      gc_files.append(path)
  with open(os.devnull, "w") as null:
    subprocess.check_output([gcov_tool] + gc_files)
  print("Generated gcov files from", gc_files)

  return True


def parse_args():
  """Parse coverage arguments."""
  from argparse import ArgumentParser
  from argparse import ArgumentDefaultsHelpFormatter
  parser = ArgumentParser(
      description="Script for collecting coverage for qwip.",
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
