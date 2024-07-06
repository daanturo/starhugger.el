#!/usr/bin/env python3

import argparse
import difflib
import itertools
import json
import re
import subprocess
import sys
from dataclasses import dataclass

# #
# import time
# start_time = time.time()


@dataclass
class GreppedLine:
    s: str
    file: str
    line_num: int

    # Expected flags: "--no-heading" "--with-filename" "--line-number"
    def __init__(self, in_string: str):
        split = in_string.split(":", maxsplit=2)
        [self.file, line_num_s, self.s] = split
        self.line_num = int(line_num_s)

    def get_line_num(self):
        return self.line_num

    def get_file(self):
        return self.file


# arg_parser = argparse.ArgumentParser()
# arg_parser.add_argument("grep-command-arguments")


args = json.loads(sys.argv[1])

compare_text = args["compare-text"]
assert isinstance(compare_text, str)

cmd_args = args["command-arguments"]
assert isinstance(cmd_args, list)

max_lines = args["max-lines"]
assert isinstance(max_lines, int)


ignored_keywords = args["ignored-keywords"]

ignored_kw_re = "|".join(f"^[ \t]*{kw}\\b" for kw in ignored_keywords)
# main function isn't interesting here
ignored_kw_re = f"{ignored_kw_re}|[ \t]main[ \t(]"
ignored_kw_re = re.compile(ignored_kw_re)

proc = subprocess.run(cmd_args, stdout=subprocess.PIPE)
grepped = proc.stdout.decode("utf-8")

if proc.returncode != 0:
    print(grepped)
    sys.exit(proc.returncode)

string_lines = grepped.split("\n")

grepped_lines = [
    GreppedLine(line)
    for line in string_lines
    if
    (
        # format: file-path:line-number:string
        line.count(":") >= 2
        # No known flags to ignore this (https://github.com/BurntSushi/ripgrep/discussions/1779)
        and (not ("[Omitted long " in line))
        # ignored keywords
        and (not re.search(ignored_kw_re, line))
    )
]


# # no duplicates
# lines = list(set(lines))

# in a same file, remove duplicate lines
dup_table = dict()
for line in grepped_lines:
    key = str([line.s, line.file])
    if not dup_table.get(key):
        dup_table[key] = line
grepped_lines = list(dup_table.values())


# sort by similarity (naive string similarity), ascending
grepped_lines.sort(
    key=lambda obj: difflib.SequenceMatcher(None, obj.s, compare_text).ratio(),
)

# take the top ones
grepped_lines = grepped_lines[-max_lines:]

groups_sorted = [
    # re-sort by line number
    (k, sorted(g, key=GreppedLine.get_line_num))
    for k, g in itertools.groupby(
        # sort by files first
        sorted(grepped_lines, key=GreppedLine.get_file),
        key=GreppedLine.get_file,
    )
]

for filename, grepped_lines in groups_sorted:
    print(filename)
    for line in grepped_lines:
        print(line.s)
    print("")

# print("Execution takes: %s seconds" % (time.time() - start_time))
