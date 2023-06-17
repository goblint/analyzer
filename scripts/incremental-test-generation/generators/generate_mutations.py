# Generate all possible mutations of a program and
# Assume there is a meta.yaml file with content "n: >=0"

import argparse
import json
import os
import re
import subprocess
import sys
import yaml
sys.path.insert(0, "..")
from util.util import *

def generate_mutations(program_path, clang_tidy_path, meta_path, mutations):
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        index: int = yaml_data[META_N]

    if mutations.rfb:
        index = _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.rfb_s, index)
    if mutations.uoi:
        index = _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.uoi_s, index)
    if mutations.ror:
        index = _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.ror_s, index)
    if mutations.cr:
        index = _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.cr_s, index)
    if mutations.rt:
        index = _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.rt_s, index)
    if mutations.lcr:
        index = _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.lcr_s, index)

    return index

def _iterative_mutation_generation(program_path, clang_tidy_path, meta_path, mutation_name, index):
    print(SEPERATOR)
    print(f"[{Generate_Type.MUTATION.value}] {mutation_name}")
    lineGroups = _get_line_groups(clang_tidy_path, mutation_name, program_path, index)
    for lines in lineGroups:
        index += 1
        new_path = make_program_copy(program_path, index)
        if mutation_name == Mutations().rt_s:
            # When Remove Thread create wrapper an then apply the mutations
            if len(lines) != 1:
                # Needed to prevent conflicts on generating wrappers
                print(f"{COLOR_RED}ERROR When applying remove_thread there always should be exactly one line{COLOR_RESET}")
            function_name = _get_thread_function_name(clang_tidy_path, lines, new_path, index)
            _wrap_thread_function(clang_tidy_path, new_path, function_name, index)
        _apply_mutation(clang_tidy_path, mutation_name, lines, new_path, index)
        _write_meta_data(meta_path, index, mutation_name, lines)
    return index

def _get_line_groups(clang_tidy_path, mutation_name, program_path, index):
    command = [
    clang_tidy_path,
    "-checks=-*,readability-" + mutation_name,
    program_path,
    "--"
    ]

    result = subprocess.run(command, text=True, capture_output=True)
    print(f"[MUTATION][CHECK] Check mutation {mutation_name}")
    if result.returncode != 0:
        print(result.stdout)
        print(result.stderr)
        print(f"{COLOR_RED}ERROR Running Clang{COLOR_RESET}")
        sys.exit(-1)

    line_groups = []
    pattern = r":(\d+):.*\[readability-" + mutation_name + r"\]"
    macro_pattern = r"\[MACRO\]\[(.*?)\]"
    macro_lines = {}

    for line in result.stdout.splitlines():
        match = re.search(pattern, line)
        if match:
            macro_match = re.search(macro_pattern, line)
            if macro_match:
                macro_name = macro_match.group(1)
                line_number = int(match.group(1))
                if macro_name not in macro_lines:
                    macro_lines[macro_name] = [line_number]
                else:
                    macro_lines[macro_name].append(line_number)
            else:
                line_groups.append([int(match.group(1))])

    for macro_name, lines in macro_lines.items():
        line_groups.append(lines)

    # Remove duplicate line groups
    line_groups = [list(x) for x in set(tuple(x) for x in line_groups)]

    print(f"[MUTATION][CHECK RESULT] Mutation {mutation_name} can be applied to lines {line_groups}")
    return sorted(line_groups, key=lambda x: x[0])

def _apply_mutation(clang_tidy_path, mutation_name, lines, program_path, index):
    lines_mapped = [[x,x] for x in lines]
    line_filter = [{"name": program_path, "lines": lines_mapped}]
    line_filter_json = json.dumps(line_filter)
    command = [
        clang_tidy_path,
        "-checks=-*,readability-" + mutation_name,
        "-fix",
        "--fix-errors",
        "-line-filter=" + line_filter_json,
        program_path,
        "--"
    ]

    result = subprocess.run(command, text=True, capture_output=True)
    if result.returncode == 0:
        print(f"{COLOR_GREEN}[{index}] Finished mutation:{COLOR_RESET} {mutation_name} on lines {lines}")
    else:
        print(result.stdout)
        print(result.stderr)
        print(f"{COLOR_RED}ERROR Running Clang{COLOR_RESET}")
        sys.exit(-1)

def _get_thread_function_name(clang_tidy_path, lines, program_path, index):
    lines_mapped = [[x,x] for x in lines]
    line_filter = [{"name": program_path, "lines": lines_mapped}]
    line_filter_json = json.dumps(line_filter)
    command = [
        clang_tidy_path,
        "-checks=-*,readability-" + Mutations().rt_s,
        "-line-filter=" + line_filter_json,
        program_path,
        "--"
    ]
    result = subprocess.run(command, text=True, capture_output=True)
    print(f"[{index}][WRAP] Check function name for wrapping thread function")
    if result.returncode != 0:
        print(result.stdout)
        print(result.stderr)
        print(f"{COLOR_RED}ERROR Running Clang{COLOR_RESET}")
        sys.exit(-1)

    function_name_pattern = r"\[FUNCTION_NAME\]\[(.*?)\]"
    function_name = None

    for line in result.stdout.splitlines():
        function_name_match = re.search(function_name_pattern, line)
        if function_name_match:
            function_name = function_name_match.group(1)
            break

    print(f"[{index}][WRAP RESULT] Found the thread function name {function_name}")
    return function_name

def _wrap_thread_function(clang_tidy_path, program_path, function_name, index):
    if function_name == None:
        print(f"{COLOR_YELLOW}[{index}][WRAP FIX] No function name was provided. Hope the program will compile without wrapping{COLOR_RESET}")
        return

    check_options = {"CheckOptions": {"readability-remove-thread-wrapper.WrapFunctionName": function_name}}
    check_options_json = json.dumps(check_options)
    command = [
        clang_tidy_path,
        "-checks=-*,readability-remove-thread-wrapper",
        "-config=" + check_options_json,
        "-fix",
        program_path,
        "--"
    ]
    result = subprocess.run(command, text=True, capture_output=True)
    print(f"[{index}][WRAP FIX] Apply the wrapping of {function_name}")
    if result.returncode != 0:
        print(result.stdout)
        print(result.stderr)
        print(f"{COLOR_RED}ERROR Running Clang{COLOR_RESET}")
        sys.exit(-1)

def _write_meta_data(meta_path, index, mutation_name, lines):
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    yaml_data[META_N] = index
    yaml_data[f"p_{index}"] = {
        META_TYPE: Generate_Type.MUTATION.value,
        META_SUB_TYPE: mutation_name,
        META_LINES: lines
    }
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)

def add_mutation_options(parser):
    parser.add_argument("-rfb", "--remove-function-body", action="store_true", help="Option for \"remove function body\" mutation")
    parser.add_argument("-uoi", "--unary-operator-inversion", action="store_true", help="Option for \"unary operator inversion\" mutation")
    parser.add_argument("-ror", "--relational-operator-replacement", action="store_true", help="Option for \"relational operator replacement\" mutation")
    parser.add_argument("-cr", "--constant-replacement", action="store_true", help="Option for \"constant replacement\" mutation")
    parser.add_argument("-rt", "--remove-thread", action="store_true", help="Option for \"remove thread\" mutation")
    parser.add_argument("-lcr", "--logical-connector-replacement", action="store_true", help="Option for \"logical connector replacement\" mutation")

def get_mutations_from_args(args):
    return Mutations(args.remove_function_body, args.unary_operator_inversion,
                                 args.relational_operator_replacement, args.constant_replacement,
                                 args.remove_thread, args.logical_connector_replacement)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate all possible mutations of a program.")
    parser.add_argument("program", help="Path to the C program")
    parser.add_argument("clang_tidy", help="Path to the modified clang-tidy executable")
    parser.add_argument("meta", help="Path to the meta data file")
    add_mutation_options(parser)

    args = parser.parse_args()
    mutations = get_mutations_from_args(args)
    generate_mutations(args.program, args.clang_tidy, args.meta, mutations)
