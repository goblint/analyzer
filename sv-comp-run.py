#!/usr/bin/python3

import sys
import glob
import subprocess
import re
import collections
import os.path
import shlex
import yaml # pip3 install pyyaml


OVERVIEW = False # with True Goblint isn't executed
GOBLINT_COMMAND = "./goblint --enable ana.sv-comp --disable ana.int.trier --enable ana.int.enums --enable ana.int.interval --sets solver td3 --enable exp.widen-context {code_filename}"
TIMEOUT = 30 # with some int that's Goblint timeout for single execution


def str2bool(s):
    return {
        "true": True,
        "false": False
    }[s]

def extract_bool(p, s):
    m = re.search(p, s)
    return str2bool(m.group(1)) if m else None


stats = collections.defaultdict(int)
try:
    set_filename = sys.argv[1]
    with open(set_filename) as set_file:
        task_filenames = []
        for pattern in set_file:
            pattern = pattern.strip()
            if pattern:
                pattern = os.path.join(os.path.dirname(set_filename), pattern)
                for task_filename in glob.iglob(pattern):
                    task_filenames.append(task_filename)

        for task_filename in sorted(task_filenames):
            print(f"{task_filename}: ", end="", flush=True)

            if task_filename.endswith(".yml"):
                with open(task_filename) as task_file:
                    y = yaml.safe_load(task_file)

                    code_filename = y["input_files"]
                    code_filename = os.path.join(os.path.dirname(task_filename), code_filename)

                    expected = None
                    for y_property in y["properties"]:
                        if y_property["property_file"] == "../properties/unreach-call.prp":
                            expected = y_property["expected_verdict"]
            else:
                code_filename = task_filename
                expected = extract_bool(r"_(false|true)-unreach-call", task_filename)

            if OVERVIEW:
                actual = None
            else:
                try:
                    p = subprocess.run(shlex.split(GOBLINT_COMMAND.format(code_filename=code_filename)), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="utf-8", timeout=TIMEOUT)
                    if "Fatal error: exception " in p.stdout:
                        print(p.stdout)
                        exit(1)
                    actual = extract_bool(r"SV-COMP \(unreach-call\): (false|true)", p.stdout)
                except subprocess.TimeoutExpired:
                    actual = "timeout"

            if "p" in locals(): # sometimes on timeout p is declared, sometimes isn't
                missing_funcs = False
                for m in re.finditer(r"Function definition missing for (__VERIFIER_\S+)", p.stdout):
                    missing_funcs = True
                    print(f"MISSING FUNC {m.group(1)}")

                if missing_funcs:
                    sys.exit(1)

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

except KeyboardInterrupt:
    pass
finally:
    print()
    print("-" * 80)
    for text, count in stats.items():
        print(f"{text}: {count}")
    print("-" * 80)
