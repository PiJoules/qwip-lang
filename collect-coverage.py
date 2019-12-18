"""Collect code coverage information. Run from the project BUILD directory."""

from __future__ import print_function

import distutils.spawn
import os
import os.path
import subprocess

DEFAULT_LCOV_TOOL = "lcov"
DEFAULT_GCOV_TOOL = "gcov"


def extract_line_counts(base_lcov_cmd):
  """Get the coverage as a percentage using --summary. It will be formatted like:
  Reading tracefile coverage.info
  Summary coverage rate:
    lines......: 100.0% (2111 of 2111 lines)
    functions..: 97.3% (401 of 412 functions)
    branches...: no data found
  """
  output = subprocess.check_output(base_lcov_cmd +
                                   ["--summary", "coverage.info"])
  lines = output.split("\n")

  # NOTE: If any of these assertions are failed, be sure to check the output of the command because it functionally may have changed.
  assert lines[0] == "Reading tracefile coverage.info"
  assert lines[1] == "Summary coverage rate:"

  # Just check the 2nd to last and 4th to last numbers in a string split for the count and total number of lines.
  lines_line = lines[2].strip()
  parts = lines_line.split(
      "(")  # ['lines......: 100.0% ', '2111 of 2111 lines)']
  parts = parts[1].split()  # ['2111', 'of', '2111', 'lines)']
  line_count = parts[0]
  total_lines = parts[2]

  # The coverage percentage is just line_count / total_lines * 100
  return line_count, total_lines


def main(lcov_tool=DEFAULT_LCOV_TOOL, gcov_tool=DEFAULT_GCOV_TOOL, **kwargs):
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

  # Get the coverage as a percentage using --summary. It will be formatted like:
  if kwargs["fail_on_missing_coverage"]:
    line_count, total_lines = extract_line_counts(base_lcov_cmd)
    if line_count != total_lines:
      print("Line code coverage is not 100%!")
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
  parser.add_argument(
      "--fail-on-missing-coverage",
      default=False,
      action="store_true",
      help="Fail early with exit code 1 if the coverage collected was not 100%."
  )
  return parser.parse_args()


if __name__ == "__main__":
  import sys
  args = parse_args()
  if not main(**vars(args)):
    sys.exit(1)
