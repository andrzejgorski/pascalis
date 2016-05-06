#!/usr/bin/env python
import subprocess
import re

tests_raw = subprocess.check_output(["ls", "tests"])
tests = re.sub("[^\w]", " ",  tests_raw).split()


OUTPUTS = {
    'program': "1",
}


def decorate_green(value):
    return '\033[1;32m{}\033[1;m'.format(value)


def decorate_red(value):
    return '\033[1;31m{}\033[1;m'.format(value)


def decorate_yellow(value):
    return '\033[1;33m{}\033[1;m'.format(value)


for test in tests:
    output = subprocess.check_output(["./TestPascalis", "-s", "tests/{}".format(test)])
    try:
        OUTPUTS[test]
    except KeyError:
        print "Program {} results {}".format(decorate_yellow(test), output)
        print "There is defaule value for this program"
    else:
        if output == OUTPUTS[test]:
            print "Test {} is {}.".format(test, decorate_green("ok"))
        else:
            print "Test {} is {}.".format(test, decorate_red("failed"))
            print "output = {}, excepcted = {}".format(output, OUTPUTS[test])
