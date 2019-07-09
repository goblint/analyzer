#!/usr/bin/python3

import sys
import glob
import subprocess
import re
import collections
import os.path
import shlex


overview = True


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
        pattern = os.path.join(os.path.dirname(set_filename), pattern)
        if pattern:
            for code_filename in glob.iglob(pattern):
                code_filenames.append(code_filename)

    for code_filename in sorted(code_filenames):
        print(f"{code_filename}: ", end="", flush=True)
        expected = extract_bool(r"_(false|true)-unreach-call", code_filename)

        if overview:
            actual = None
        else:
            try:
                # p = subprocess.run(f"~/Desktop/sv-comp/goblint/goblint --enable ana.sv-comp --enable dbg.debug {code_filename}", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8")
                # p = subprocess.run(f"~/Desktop/sv-comp/goblint/goblint --enable ana.sv-comp --enable dbg.debug --set ana.activated[+] \"'var_eq'\" --enable ana.int.interval {code_filename}", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8")
                # p = subprocess.run(shlex.split(f"/home/simmo/Desktop/sv-comp/goblint/goblint --enable ana.sv-comp --enable dbg.debug --set ana.activated[+] \"'var_eq'\" --enable ana.int.interval {code_filename}"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8", timeout=10)
                p = subprocess.run(shlex.split(f"/home/simmo/Desktop/sv-comp/goblint/goblint --enable ana.sv-comp --enable dbg.debug --set ana.activated[+] \"'var_eq'\" --set ana.activated[+] \"'symb_locks'\" --set ana.activated[+] \"'thread'\" --set ana.activated[+] \"'region'\" --enable ana.int.interval {code_filename}"), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8", timeout=30)
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

    print("-" * 80)
    for text, count in stats.items():
        print(f"{text}: {count}")