#!/usr/bin/python3

from pathlib import Path
import re
import shlex
import json


tests_root_path = Path("./tests/regression")

# copied from options
activated_default = set([
    "expRelation", "base", "threadid", "threadflag", "threadreturn",
    "escape", "mutexEvents", "mutex", "access", "mallocWrapper", "mhp",
    "assert"
])

for test_path in tests_root_path.glob("*/*.c"):
    # print(test_path)
    with test_path.open() as test_file:
        line = test_file.readline().strip()
        # print(line)
        m = re.match(r"^//.*PARAM.*:\s*(.*)$", line)
        if m is not None:
            param = m.group(1)
            params = shlex.split(param)
            if "ana.activated" in params:
                activated_i = params.index("ana.activated")
                activated_str = params[activated_i + 1]
                activated_str = activated_str.replace("'","\"") # silly Goblint JSON
                # print(activated)
                activated = set(json.loads(activated_str))
                added = activated - activated_default
                removed = activated_default - activated
                # print(added, removed)
                if added or removed:
                    print(test_path)
                    if added:
                        # print(f"  added: {added}")
                        args_str = ""
                        for analysis in added:
                            args_str += f" --set ana.activated[+] {analysis}"
                        print(f"  added:{args_str}")
                    if removed:
                        # print(f"  removed: {removed}")
                        args_str = ""
                        for analysis in removed:
                            args_str += f" --set ana.activated[-] {analysis}"
                        print(f"  removed:{args_str}")
