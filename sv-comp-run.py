#!/usr/bin/python3

import sys
import glob
import subprocess
import re
import collections
import os.path


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

    for pattern in set_file:
        pattern = pattern.strip()
        pattern = os.path.join(os.path.dirname(set_filename), pattern)
        if pattern:
            for code_filename in glob.iglob(pattern):
                print(f"{code_filename}: ", end="", flush=True)
                expected = extract_bool(r"_(false|true)-unreach-call", code_filename)

                p = subprocess.run(f"~/Desktop/sv-comp/goblint/goblint --enable ana.sv-comp --enable dbg.debug {code_filename}", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8")
                actual = extract_bool(r"__VERIFIER_error unreach2: (false|true)", p.stdout)

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
                else:
                    text = f"INCORRECT expected {expected}, actual {actual}"

                print(text)
                stats[text] += 1

    print("-" * 80)
    for text, count in stats.items():
        print(f"{text}: {count}")