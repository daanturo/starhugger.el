#!/usr/bin/env python3

import difflib
import json
import re
import subprocess
import sys

# #
# import time
# start_time = time.time()

args = json.loads(sys.argv[1])

cmd_args = args["command-arguments"]

compare_text = args["compare-text"]
assert isinstance(compare_text, str)

max_lines = args["max-lines"]
assert isinstance(max_lines, int)

ignored_keywords = args["ignored-keywords"]
ignored_kw_re = "|".join(f"^[ \t]*{kw}\b" for kw in ignored_keywords)
ignored_kw_re = f"{ignored_kw_re}|[^_-]main[^_-]"
ignored_kw_re = re.compile(ignored_kw_re)


grepped = subprocess.run(cmd_args, stdout=subprocess.PIPE).stdout.decode("utf-8")

lines = grepped.splitlines()
# no empty
lines = [line for line in lines if line != ""]
# no duplicates
lines = list(set(lines))
# ignored keywords
lines = [line for line in lines if not re.search(ignored_kw_re, line)]
# sort by similarity (native string similarity), ascending
lines.sort(
    key=lambda arg: difflib.SequenceMatcher(None, arg, compare_text).ratio(),
)

# truncate
lines = lines[-max_lines:]

# push elements with indents to the start
lines.sort(key=lambda arg: re.search(r"[^ \t]", arg).end(), reverse=True)

for line in lines:
    print(line)

# print("Execution takes: %s seconds" % (time.time() - start_time))
