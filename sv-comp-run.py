#!/usr/bin/python3

import sys
import glob
import subprocess
import re
import collections
import os.path
import shlex


OVERVIEW = False # with True Goblint isn't executed
GOBLINT_COMMAND = "./goblint --enable ana.sv-comp --enable ana.int.interval {code_filename}"
TIMEOUT = None # with some int that's Goblint timeout for single execution


def str2bool(s):
    return {
        "true": True,
        "false": False
    }[s]

def extract_bool(p, s):
    m = re.search(p, s)
    return str2bool(m.group(1)) if m else None

set_filename = sys.argv[1]
with open(set_filename) as set_file:
    stats = collections.defaultdict(int)

    code_filenames = []
    for pattern in set_file:
        pattern = pattern.strip()
        if pattern:
            pattern = os.path.join(os.path.dirname(set_filename), pattern)
            for code_filename in glob.iglob(pattern):
                code_filenames.append(code_filename)

    for code_filename in sorted(code_filenames):
        print(f"{code_filename}: ", end="", flush=True)
        # TODO: handle .yml task definitions
        expected = extract_bool(r"_(false|true)-unreach-call", code_filename)

        if OVERVIEW:
            actual = None
        else:
            try:
                p = subprocess.run(shlex.split(GOBLINT_COMMAND.format(code_filename=code_filename)), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8", timeout=TIMEOUT)
                actual = extract_bool(r"SV-COMP \(unreach-call\): (false|true)", p.stdout)
            except subprocess.TimeoutExpired:
                actual = "timeout"

        if "p" in locals(): # sometimes on timeout p is declared, sometimes isn't
            missing_funcs = False
            for m in re.finditer(r"Function definition missing for (__VERIFIER_\S+)", p.stdout):
                missing_funcs = True
                print(f"MISSING FUNC {m.group(1)}")

            if missing_funcs:
                sys.exit(0)

        text = None
        if expected is None or actual is None:
            text = f"NONE expected {expected}, actual {actual}"
        elif actual == expected:
            text = f"CORRECT {expected}"
        elif actual == "timeout":
            text = f"UNKNOWN expected {expected}, actual {actual}"
        else:
            text = f"INCORRECT expected {expected}, actual {actual}"

        print(text)
        stats[text] += 1

    # TODO: print this even when ctrl-c-ing script
    print("-" * 80)
    for text, count in stats.items():
        print(f"{text}: {count}")