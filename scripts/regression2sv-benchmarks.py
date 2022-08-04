#!/usr/bin/python3

import sys
from pathlib import Path
import re
import subprocess
import argparse
import copy
from dataclasses import dataclass
from typing import List, Optional

EXCLUDE_TASKS = [
    "04-mutex_13-failed_locking",
    "04-mutex_31-uninitialized",
    "04-mutex_49-type-invariants",
    "04-mutex_29-funstruct_rc",
    "04-mutex_30-funstruct_nr",
    "04-mutex_30-funstruct_nr",

    "06-symbeq_16-type_rc", # extern
    "06-symbeq_17-type_nr", # extern
    "06-symbeq_20-mult_accs_nr", # extern
    "06-symbeq_21-mult_accs_rc", # extern
    "10-synch_04-two_mainfuns", # no main
    "05-lval_ls_17-per_elem_simp", # no pthread include, locksmith pragma

    "09-regions_29-malloc_race_cp", # duplicate of 02/25
    "09-regions_30-list2alloc-offsets", # duplicate of 09/28
    "09-regions_31-equ_rc", # duplicate of 06/10
    "09-regions_32-equ_nr", # duplicate of 06/11
    "09-regions_34-escape_rc", # duplicate of 04/45
    "09-regions_35-list2_rc-offsets-thread", # duplicate of 09/03
    "10-synch_17-glob_fld_nr", # duplicate of 05/08
    "19-spec_02-mutex_rc", # duplicate of 04/01

    "29-svcomp_01-race-2_3b-container_of", # duplicate sv-benchmarks
    "29-svcomp_01-race-2_4b-container_of", # duplicate sv-benchmarks
    "29-svcomp_01-race-2_5b-container_of", # duplicate sv-benchmarks

    "13-privatized_02-priv_rc", # intentional data race
    "13-privatized_03-priv_inv", # no-data-race contains assert()
    "13-privatized_17-priv_interval", # duplicate of 13/01
    "13-privatized_21-publish-basic", # intentional data race
    "13-privatized_22-traces-paper", # intentional data race
    "13-privatized_23-traces-paper2", # intentional data race
    "13-privatized_26-struct_rc", # intentional data race
    "13-privatized_28-multiple-protecting2-simple", # similar to 13/27
    "13-privatized_39-traces-ex-5", # intentional data race
    "13-privatized_43-traces-mine1", # intentional data race
    "13-privatized_48-pfscan_protected_loop_minimal", # overflow, non-refining assert
    "13-privatized_49-refine-protected-loop", # overflow, non-refining assert
    "13-privatized_50-pfscan_protected_loop_minimal2", # overflow, non-refining assert
    "13-privatized_51-refine-protected-loop2", # overflow, non-refining assert

    "36-apron_12-traces-min-rpb1", # intentional data race
    "36-apron_13-traces-min-rpb2", # intentional data race
    "36-apron_14-traces-unprot", # intentional data race
    "36-apron_19-traces-other-rpb", # intentional data race
    "36-apron_42-threadenter-arg", # intentional threadenter arg as int
    "36-apron_61-branched", # intentional data race
    "36-apron_62-branched_intricate", # intentional data race
    "36-apron_63-branched-not-too-brutal", # intentional data race
]

def parse_arguments():
    parser = argparse.ArgumentParser()

    parser.add_argument("-g", "--goblint_path", dest = "goblint_path", default = ".", help="Path to Goblint root.")
    parser.add_argument("-t", "--target_path", dest = "target_path", required=True, help="Path to the regression tests.")
    parser.add_argument("-s", "--source_folder", dest = "source_folder", default = "**", help="Path to the folder wih a group of tests. Default: all folders.")

    global args
    args = parser.parse_args()

    goblint_root = Path(args.goblint_path)
    global goblint_regression
    goblint_regression = goblint_root / "tests" / "regression"

    global target_root
    target_root = Path(args.target_path)

def process_files():
    for goblint_f in sorted(goblint_regression.glob(args.source_folder+"/*.c")):
        print(goblint_f, end=": ")

        content = goblint_f.read_text()
        # handle & strip Goblint param hints
        m = re.match(r"^//(.*?)\n(.*)$", content, flags=re.DOTALL)
        if m:
            top_comment = m.group(1)
            content = m.group(2)
        else:
            top_comment = None

        if top_comment is not None:
            if "SKIP" in top_comment and goblint_f.parent.name != "36-apron": # 36-apron is skipped just for our weird regression testing purposes
                print("skip")
                continue
            elif "kernel" in top_comment:
                print("kernel")
                continue
            elif "allfuns" in top_comment:
                print("allfuns")
                continue
            elif "otherfun" in top_comment:
                print("otherfun")
                continue

        # only generate multithreaded benchmarks because c/goblint-regression/ in sv-benchmarks is included in ConcurrencySafety
        if not ("pthread_create" in content or "create_threads" in content): # 28-race_reach uses create_threads macro
            print("singlethreaded")
            continue

        task_name = Path(goblint_f.parent.name + "_" + goblint_f.name).stem
        if task_name in EXCLUDE_TASKS:
            print("exclude")
            continue

        properties = {}

        content = re.sub(r"//\s*RACE(?!!)", "// NORACE", content)
        if re.search(r"//\s*RACE!", content):
            properties["../properties/no-data-race.prp"] = False
        elif re.search(r"//\s*NORACE", content):
            # if didn't contain RACE!, must be race-free
            properties["../properties/no-data-race.prp"] = True

        if re.search(r"assert_racefree[^\n]*//\s*UNKNOWN", content):
            properties["../properties/unreach-call.prp"] = False
        elif "assert_racefree" in content:
            # if didn't contain UNKNOWN assert_racefree, must be race-free
            properties["../properties/unreach-call.prp"] = True

        handle_asserts(properties, content, task_name, top_comment)

@dataclass
class Assert:
    indent: str
    exp: str
    comment: Optional[str]

    @property
    def is_success(self) -> bool:
        """Is always succeeding."""
        return self.comment is None or \
            (self.comment is not None and "TODO" in self.comment) or \
            (self.comment is not None and "SUCCESS" in self.comment)

    @property
    def is_fail(self) -> bool:
        """Is always failing."""
        return self.comment is not None and "FAIL" in self.comment

    @property
    def is_unknown(self) -> bool:
        """Is definitely unknown."""
        return self.comment is not None and "UNKNOWN!" in self.comment

    @property
    def is_ignored(self) -> bool:
        """Is ignored assert."""
        # TODO: NOWARN should be reach_error() true
        # TODO: assert(1) should be reach_error() false (reachable)
        # TODO: each assert should also have reach_error() false variant (assertion reachable)
        return (self.comment is not None and (("UNKNOWN" in self.comment and "UNKNOWN!" not in self.comment) or "NOWARN" in self.comment)) or \
            self.exp == "1" or self.exp == "1 == 1"

ASSERT_PATTERN = re.compile(r"((?<=[\r\n;])|^)(?P<indent>[ \t]*)assert[ \t]*\((?P<exp>.*)\)[ \t]*;[ \t]*(//[ \t]*(?P<comment>.*)[ \t]*)?(\r\n|\r|\n)")

ASSERT_HEADER = """#include <assert.h>
extern void abort(void);
void reach_error() { assert(0); }
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }

"""

ASSERT_INCLUDE_PATTERN = re.compile(r"#include\s*<assert\.h>(\r\n|\r|\n)")

def handle_asserts(properties, content, task_name, top_comment):
    print()

    content = ASSERT_INCLUDE_PATTERN.sub("", content) # remove existing assert.h include to only keep the one we add from ASSERT_HEADER above

    # Split the file into parts by asserts
    codes = [] # type: List[str]
    asserts = [] # type: List[Assert]
    prev_match_end = 0
    for match in ASSERT_PATTERN.finditer(content):
        code_before = content[prev_match_end:match.start()]
        codes.append(code_before)

        a = Assert(indent=match.group("indent"), exp=match.group("exp"), comment=match.group("comment"))
        asserts.append(a)

        prev_match_end = match.end()

    code_after = content[prev_match_end:]
    codes.append(code_after)
    # content is represented by: codes[0], asserts[0], codes[1], asserts[1], ..., asserts[n], codes[n + 1]

    if asserts:
        print("  asserts:")
        for a in asserts:
            print(f"    {a}{' (ignored)' if a.is_ignored else ''}")

    # Create benchmarks for each UNKNOWN! assert
    unknown_version = 1
    for i, a in enumerate(asserts):
        # cannot base unknown_version on i (with filtering) because code_prefix/suffix still needs to use i if there are non-unknown asserts
        if not a.is_ignored and a.is_unknown:
            code_prefix = "".join(codes[:i + 1])
            code_suffix = "".join(codes[i + 1:])

            content = f"{ASSERT_HEADER}{code_prefix}{a.indent}__VERIFIER_assert({a.exp});\n{code_suffix}"
            properties["../properties/unreach-call.prp"] = False
            print(f"  false assert positive version {unknown_version}:")
            handle_properties(properties, task_name + f"_unknown_{unknown_version}_pos", content, top_comment)

            content = f"{ASSERT_HEADER}{code_prefix}{a.indent}__VERIFIER_assert(!({a.exp}));\n{code_suffix}"
            properties["../properties/unreach-call.prp"] = False
            print(f"  false assert negative version {unknown_version}:")
            handle_properties(properties, task_name + f"_unknown_{unknown_version}_neg", content, top_comment)

            unknown_version += 1

    # Create one big benchmark for all the other asserts
    content = ASSERT_HEADER
    found_true = False
    for i, a in enumerate(asserts):
        content += codes[i]
        if not a.is_ignored:
            if a.is_fail:
                content += f"{a.indent}__VERIFIER_assert(!({a.exp}));\n"
                found_true = True
            elif a.is_success:
                content += f"{a.indent}__VERIFIER_assert({a.exp});\n"
                found_true = True
    content += codes[-1]

    if found_true:
        properties["../properties/unreach-call.prp"] = True
        print(f"  true asserts version:")
        handle_properties(properties, task_name + "_true", content, top_comment)


def handle_properties(properties, task_name, content, top_comment):
    if properties:
        # print()
        for property_file, expected_verdict in properties.items():
            print(f"    {property_file}: {expected_verdict}")

        # copy file
        target_f = target_root / (task_name + ".c")
        print(f"    -> {target_f}")
        target_f.write_text(content)

        # preprocess file
        preprocessed_f = target_root / (task_name + ".i")
        print(f"    -> {preprocessed_f}")
        preprocessed_f.touch()
        with preprocessed_f.open("w") as f:
            # TODO: change -m32 to -m64
            # running gcc in target_root with relative path avoid absolute paths in preprocessor
            subprocess.run(["gcc", "-E", "-P", "-m32", str(target_f.relative_to(target_root))], stdout=f, check=True, cwd=target_root)

        # create task definition
        task_definition_f = target_root / (task_name + ".yml")
        task_definition_f.touch()
        # write yml manually to get consistent formatting
        with task_definition_f.open("w") as f:
            f.write("format_version: '2.0'\n")
            f.write("\n")
            if top_comment:
                f.write(f"# original top comment: {top_comment}\n")
            f.write(f"input_files: '{preprocessed_f.relative_to(target_root)}'\n")
            f.write("\n")
            f.write("properties:\n")
            for property_file, expected_verdict in properties.items():
                f.write(f"  - property_file: {property_file}\n")
                f.write(f"    expected_verdict: {'true' if expected_verdict else 'false'}\n")
            f.write("\n")
            f.write("options:\n")
            f.write("  language: C\n")
            f.write("  data_model: ILP32\n") # TODO: is this right for Goblint tests?

    else:
        print("no properties")

def main():
    parse_arguments()
    process_files()

if __name__ == "__main__":
    main()
