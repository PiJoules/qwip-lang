"""Script for running the examples as part of the test suite."""

from __future__ import print_function

import subprocess
import shlex
import tempfile
import os.path

SINGLE_LINE_COMMENT_START = "//"
COMPILE_CMD = "COMPILE:"
RUN_CMD = "RUN:"
CHECK_CMD = "CHECK:"
CHECK_NEXT_CMD = "CHECK-NEXT:"
EXPECT_NO_OUTPUT = "EXPECT-NO-OUTPUT"
EXPECT_COMPILE_ERROR = "EXPECT-COMPILE-ERROR"


class CheckType:
  CHECK = 1
  CHECK_NEXT = 2


def replace_placeholders(line, placeholders):
  line = shlex.split(line)
  new_cmd = []
  for token in line:
    if token in placeholders:
      new_cmd.append(placeholders[token])
    else:
      new_cmd.append(token)
  return " ".join(new_cmd)


def run_lit_test(qwip_file, qwip_exe, placeholders):
  run_cmd = None
  compile_cmd = None
  checks = []
  expect_no_output = False
  expect_compile_error = False

  for line in qwip_file:
    line = line.lstrip()
    if not line.startswith(SINGLE_LINE_COMMENT_START):
      continue

    line = line[len(SINGLE_LINE_COMMENT_START):]
    line = line.lstrip()
    if line.startswith(COMPILE_CMD):
      line = line[len(COMPILE_CMD):]
      if compile_cmd is not None:
        raise RuntimeError(
            "Found another compile command: {}\nWhen one was already set: {}\n".
            format(compile_cmd, line))
      compile_cmd = line
    elif line.startswith(RUN_CMD):
      line = line[len(RUN_CMD):]
      if compile_cmd is None:
        raise RuntimeError("Expected a COMPILE command before running: " + line)
      if run_cmd is not None:
        raise RuntimeError(
            "Found another run command: {}\nWhen one was already set: {}\n".
            format(run_cmd, line))
      run_cmd = line
    elif line.startswith(CHECK_CMD):
      line = line[len(CHECK_CMD):]
      checks.append((line, CheckType.CHECK))
    elif line.startswith(CHECK_NEXT_CMD):
      line = line[len(CHECK_NEXT_CMD):]
      checks.append((line, CheckType.CHECK_NEXT))
    elif line.strip() == EXPECT_NO_OUTPUT:
      expect_no_output = True
    elif line.strip() == EXPECT_COMPILE_ERROR:
      expect_compile_error = True
    else:
      # A regular comment
      continue

  assert compile_cmd, "No COMPILE command found"
  assert bool(
      checks
  ) ^ expect_no_output, "No either EXPECT-NO-CHECKS or at least one CHECK"

  # Substitute placeholders
  compile_cmd = replace_placeholders(compile_cmd, placeholders)

  if expect_compile_error:
    try:
      subprocess.check_output(shlex.split(compile_cmd),
                              stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
      compile_out = e.output
    else:
      raise RuntimeError(
          "Expected '{}' to throw an error, but it didn't".format(compile_cmd))
  else:
    compile_out = subprocess.check_output(shlex.split(compile_cmd),
                                          stderr=subprocess.STDOUT)

  if run_cmd:
    run_cmd = replace_placeholders(run_cmd, placeholders)
    run_out = subprocess.check_output(shlex.split(run_cmd),
                                      stderr=subprocess.STDOUT)
  else:
    run_out = ""

  if expect_no_output:
    output = compile_out + run_out
    assert not output, "Expected no output for the run. Found:\n{}".format(
        output)
    return

  num_found_checks = 0
  i = 0
  output_lines = (compile_out + run_out).split("\n")
  while i < len(output_lines):
    line = output_lines[i]
    check_line, check_type = checks[num_found_checks]
    check_line = check_line.strip()
    if check_line in line:
      num_found_checks += 1
      if num_found_checks >= len(checks):
        break
    elif check_type == CheckType.CHECK_NEXT:
      if num_found_checks:
        raise RuntimeError(
            "Did not find CHECK-NEXT: {}\nAfter previous CHECK: {}".format(
                check_line, checks[num_found_checks - 1][0]))
      else:
        raise RuntimeError(
            "Did not find CHECK-NEXT at start of output: {}".format(check_line))

    i += 1
  else:
    raise RuntimeError("Did not find CHECK: " + checks[num_found_checks])


def make_tmp_filename():
  with tempfile.NamedTemporaryFile() as tmp_file:
    return tmp_file.name


def run_lit_test_filename(qwip_filename, qwip_exe):
  if not os.path.splitext(qwip_filename)[1] == ".qw":
    return
  print("Running lit test on", qwip_filename)
  with open(qwip_filename, "r") as qwip_file:
    placeholders = {
        "%qwip": qwip_exe,
        "%src": qwip_filename,
        "%tmp": make_tmp_filename(),
    }
    run_lit_test(qwip_file, qwip_exe, placeholders)


def parse_args():
  """Parse command line arguments."""
  from argparse import ArgumentParser
  from argparse import ArgumentDefaultsHelpFormatter
  parser = ArgumentParser(
      description="Python script for collecting coverage for qwip.",
      formatter_class=ArgumentDefaultsHelpFormatter)
  parser.add_argument(
      "target",
      default=None,
      help="Run the lit test on one specific file or directory.")
  parser.add_argument("-q",
                      "--qwip",
                      required=True,
                      help="Path to qwip executable.")
  parser.add_argument("--valgrind",
                      default=False,
                      action="store_true",
                      help="Use valgrind")
  return parser.parse_args()


def main(args):
  qwip_exe = args.qwip
  if args.valgrind:
    qwip_exe = "valgrind --leak-check=full --error-exitcode=1 " + qwip_exe
  if os.path.isfile(args.target):
    run_lit_test_filename(args.target, qwip_exe)
  elif os.path.isdir(args.target):
    for root, _, files in os.walk(args.target):
      for filename in files:
        qwip_filename = os.path.join(root, filename)
        run_lit_test_filename(qwip_filename, qwip_exe)
  else:
    raise RuntimeError("Non-existant path: " + args.target)
  return True


if __name__ == "__main__":
  assert main(parse_args())
