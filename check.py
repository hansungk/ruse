#!/usr/bin/env python3
import re
import sys
import subprocess


def test(binname, filename):
    beacon_list = []
    exit_beacon = 0

    line_number = 1
    for line in open(filename, 'r'):
        if line_number == 1:
            match = re.search("// exit", line)
            if match:
                exit_beacon_string = match.string[match.start():][len("// exit "):-1]
                exit_beacon = int(exit_beacon_string)

        match = re.search("//~error:", line)
        if match:
            comment_string = match.string[match.start():]
            # strip '//~error: ' and '\n'
            just_beacon = comment_string[len("//~error: "):-1]
            beacon_list.append((line_number, just_beacon))
        line_number = line_number + 1

    # Run compiler and capture output
    error_list = []

    r = subprocess.run([binname, filename], stdout=None, stderr=subprocess.PIPE, text=True)
    if r.returncode != 0 and r.returncode != 1:
        print('\033[0;31mabort\033[0m {} ({})'.format(filename, r.returncode))
        return

    for line in r.stderr.splitlines():
        match = re.search("error:", line)
        splits = line.split(':')
        if not match or len(splits) < 5:
            print('\033[0;31mcodegen\033[0m {} ({})'.format(filename, line))
            return
        line_number = int(splits[1])
        # strip 'error: '
        error_string = match.string[match.start():][len("error: "):]
        error_list.append((line_number, error_string))

    success = True
    i = 0
    j = 0
    while i < len(error_list) and j < len(beacon_list):
        this_error = error_list[i]
        this_beacon = beacon_list[j]
        if this_error[0] == this_beacon[0]:
            if this_error[1] != this_beacon[1]:
                success = False
                print('line {}: expected: {}'.format(this_beacon[0], this_beacon[1]))
                print('line {}: got:      {}'.format(this_error[0], this_error[1]))
            i = i + 1
            j = j + 1
        elif this_error[0] < this_beacon[0]:
            success = False
            print('line {}: got:      {}'.format(this_error[0], this_error[1]))
            i = i + 1
        else:
            success = False
            print('line {}: expected: {}'.format(this_beacon[0], this_beacon[1]))
            j = j + 1
    # handle any remaining
    while i < len(error_list):
        success = False
        this_error = error_list[i]
        print('line {}: got:      {}'.format(this_error[0], this_error[1]))
        i = i + 1
    while j < len(beacon_list):
        success = False
        this_beacon = beacon_list[j]
        print('line {}: expected: {}'.format(this_beacon[0], this_beacon[1]))
        j = j + 1

    if not success:
        print('\033[0;31msema\033[0m {}'.format(filename))
        return

    r = subprocess.run('./out')
    if exit_beacon != r.returncode:
        print('\033[0;31mrun\033[0m {} (expected {}, got {})'
                .format(filename, exit_beacon, r.returncode))
        return
    print('\033[0;32mpass\033[0m {}'.format(filename))


if __name__ == "__main__":
    if len(sys.argv) > 1:
        test('build/ruse', sys.argv[1])
        sys.exit(0)

    test('build/ruse', 'test/simple.ruse')
    test('build/ruse', 'test/return.ruse')
    test('build/ruse', 'test/if.ruse')
    test('build/ruse', 'test/typecheck.ruse')
    test('build/ruse', 'test/struct.ruse')
    test('build/ruse', 'test/struct2.ruse')
    test('build/ruse', 'test/func.ruse')
    test('build/ruse', 'test/array.ruse')
    test('build/ruse', 'test/string.ruse')
    test('build/ruse', 'test/ffi.ruse')
    test('build/ruse', 'test/codegen.ruse')
    test('build/ruse', 'test/codegen_struct.ruse')
    test('build/ruse', 'test/ref.ruse')
