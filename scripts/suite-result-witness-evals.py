#!/usr/bin/python3

# checks evals count changes after ./scripts/update_suite.rb -w


from pathlib import Path
import re


suite_result_path = Path("./tests/suite_result")

def extract_evals(path):
    with path.open() as f:
        s = f.read()
        m = re.search(r"evals = (\d+)", s)
        if m is not None:
            return int(m.group(1))
        else:
            return None

count_same = 0
count_better = 0
count_worse = 0

for after_path in suite_result_path.glob("*/*.warn.txt2"):
    before_path = after_path.with_suffix(".txt")
    print(after_path, end=" ")
    before_evals = extract_evals(before_path)
    after_evals = extract_evals(after_path)
    print(before_evals, after_evals)
    if before_evals is not None and after_evals is not None:
        if before_evals == after_evals:
            count_same += 1
        elif before_evals > after_evals:
            count_better += 1
        else:
            count_worse += 1
            print(f"WORSE! by {after_evals - before_evals}")

print(f"same: {count_same}")
print(f"better: {count_better}")
print(f"worse: {count_worse}")
