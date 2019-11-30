from __future__ import print_function

import shlex
import subprocess
import sys


class MismatchException(Exception):

  def __init__(self, cmd, found_out, expected_out):
    msg = """
Outputs do not match
Running command: {cmd}
Found:
{found_out}
---
Expected:
{expected_out}
""".format(**locals())
    super(MismatchException, self).__init__(msg)


def main(argv):
  cmd = argv[1]
  expected_out = argv[2]

  split_cmd = shlex.split(cmd)
  found_out = subprocess.check_output(split_cmd)

  if found_out != expected_out:
    raise MismatchException(split_cmd, found_out, expected_out)
  return True


if __name__ == "__main__":
  assert main(sys.argv)
