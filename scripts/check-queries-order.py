#!/usr/bin/env python3
"""Check that each number appears only once in the order function in queries.ml.

If two query constructors are assigned the same order number, Goblint will
compile but then segfault at runtime due to type-unsafe operations in the
query system.
"""

import re
import sys
from pathlib import Path


def main():
    queries_ml = Path(__file__).resolve().parent.parent / "src" / "domains" / "queries.ml"

    if not queries_ml.exists():
        print(f"ERROR: {queries_ml} not found", file=sys.stderr)
        sys.exit(1)

    content = queries_ml.read_text()

    # Find the order function body.
    # The function starts with "let order = function" and ends at the next
    # top-level "let" binding at the same indentation level inside the module,
    # or at the end of the file if no subsequent binding exists.
    order_match = re.search(
        r"let order = function\n(.*?)(?=\n\s{2,}let\b|\Z)",
        content,
        re.DOTALL,
    )

    if not order_match:
        print(
            "ERROR: Could not find 'let order = function' in queries.ml",
            file=sys.stderr,
        )
        sys.exit(1)

    order_body = order_match.group(1)

    # Extract all "-> N" patterns where N is an integer.
    # Allow for optional trailing whitespace or comments after the number.
    numbers = re.findall(r"->\s*(\d+)\b", order_body)

    if not numbers:
        print(
            "ERROR: Could not extract numbers from order function in queries.ml",
            file=sys.stderr,
        )
        sys.exit(1)

    # Check for duplicates.
    seen: set[int] = set()
    duplicates: set[int] = set()
    for num_str in numbers:
        num = int(num_str)
        if num in seen:
            duplicates.add(num)
        else:
            seen.add(num)

    if duplicates:
        dups_sorted = sorted(duplicates)
        print(
            f"ERROR: Duplicate numbers in order function in queries.ml: {dups_sorted}",
            file=sys.stderr,
        )
        sys.exit(1)

    print(f"OK: All {len(numbers)} order values in queries.ml are unique.")


if __name__ == "__main__":
    main()
