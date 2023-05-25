# Generate all possible mutations of a program and
# Assume there is a meta.yaml file with content "n: >=0"

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import yaml

generate_type_mutation = "MUTATION"
seperator = "--------------------"

class Mutations:
    def __init__(self, rfb=False, uoi=False, ror=False, cr=False, rt=False, lcr=False):
        self.rfb = rfb
        self.rfb_s = "remove-function-body"
        self.uoi = uoi
        self.uoi_s = "unary-operator-inversion"
        self.ror = ror
        self.ror_s = "relational-operator-replacement"
        self.cr = cr
        self.cr_s = "constant-replacement"
        self.rt = rt
        self.rt_s = "remove-thread"
        self.lcr = lcr
        self.lcr_s = "logical-connector-replacement"

def generate_mutations(program_path, clang_tidy_path, meta_path, mutations):
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        index: int = yaml_data["n"]

    if mutations.rfb:
        index = _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.rfb_s, index)
    if mutations.uoi:
        index = _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.uoi_s, index)
    if mutations.ror:
        index = _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.ror_s, index)
    if mutations.cr:
        index = _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.cr_s, index)
    if mutations.rt:
        index = _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.rt_s, index)
    if mutations.lcr:
        index = _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutations.rt_s, index)

def _basic_mutation_generation(program_path, clang_tidy_path, meta_path, mutation_name, index):
    print(seperator)
    print(f"[{generate_type_mutation}] {mutation_name}")
    lineGroups = _get_line_groups(clang_tidy_path, mutation_name, program_path)
    for lines in lineGroups:
        index += 1
        new_path = _make_copy(program_path, index)
        _apply_mutation(clang_tidy_path, mutation_name, lines, new_path, index)
        _write_meta_data(meta_path, new_path, index, mutation_name, lines)
    return index

def _make_copy(program_path, index):
    new_path = program_path.rsplit('.', 1)[0] + '_' + str(index) + '.c'
    shutil.copy2(program_path, new_path)
    return new_path

def _get_line_groups(clang_tidy_path, mutation_name, program_path):
    #TODO Handle [MACRO] tags
    command = [
    clang_tidy_path,
    "-checks=-*,readability-" + mutation_name,
    program_path,
    "--"
    ]

    result = subprocess.run(command, text=True, capture_output=True)
    print(f"[CHECK] Check mutation {mutation_name} with return code {result.returncode}")
    if result.returncode != 0:
        print(result.stdout)
        print("ERROR Running Clang")
        sys.exit(-1)

    line_groups = []
    pattern = r":(\d+):.*\[readability-" + mutation_name + r"\]"

    for line in result.stdout.splitlines():
        match = re.search(pattern, line)
        if match:
            line_groups.append([int(match.group(1))])

    print(f"[CHECK RESULT] Mutation {mutation_name} can be applied to lines {line_groups}")
    return line_groups

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
    print(f"[{index}] Run mutation {mutation_name} on lines {lines} with return code {result.returncode}")
    if result.returncode != 0:
        print(result.stdout)
        print("ERROR Running Clang")
        sys.exit(-1)

def _write_meta_data(meta_path, new_path, index, mutation_name, lines):
    name = os.path.basename(new_path)
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    yaml_data["n"] = index
    yaml_data[name] = {
        "type": generate_type_mutation,
        "sub_type": mutation_name,
        "lines": lines
    }
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file, sort_keys=False)

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
