#!/usr/bin/python3

from pathlib import Path
import re
import sys

src_root_path = Path("./src")

goblint_lib_path = src_root_path / "goblint_lib.ml"
goblint_lib_modules = set()

with goblint_lib_path.open() as goblint_lib_file:
    for line in goblint_lib_file:
        line = line.strip()
        m = re.match(r"module (.*) = .*", line)
        if m is not None:
            module_name = m.group(1)
            goblint_lib_modules.add(module_name)

src_vendor_path = src_root_path / "vendor"
exclude_module_names = set([
    "Goblint_lib", # itself

    # executables
    "Goblint",
    "MessagesCompare",
    "PrivPrecCompare",
    "ApronPrecCompare",
    "Mainspec",

    # libraries
    "Goblint_timing",
    "Goblint_backtrace",
    "Goblint_sites",
    "Goblint_build_info",

    "MessageCategory", # included in Messages
    "PreValueDomain", # included in ValueDomain
    "SpecCore", # spec stuff
    "SpecUtil", # spec stuff
])

src_modules = set()

for ml_path in src_root_path.glob("**/*.ml"):
    if str(ml_path).startswith(str(src_vendor_path)):
        continue

    module_name = ml_path.with_suffix("").with_suffix("").name
    module_name = module_name[0].upper() + module_name[1:]
    if module_name.endswith("0") or module_name.endswith("_intf") or module_name in exclude_module_names:
        continue

    src_modules.add(module_name)

if len(src_modules) > 0:
    print(f"Modules missing from {goblint_lib_path}: {src_modules - goblint_lib_modules}")
    sys.exit(1)
