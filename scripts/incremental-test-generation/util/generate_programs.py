import argparse
import os
import shutil
import sys
sys.path.insert(0, "..")
from util.util import *
from util.add_check import add_check
from util.add_check_comments import add_check_comments
from generators.generate_mutations import *
from generators.generate_ml import *
from generators.generate_git import *

generate_type_source = "SOURCE"

def generate_programs(source_path, temp_dir, clang_tidy_path, goblint_path, apikey_path, mutations, enable_mutations, enable_ml, enable_git, ml_count, ml_select, ml_interesting, ml_16k, git_start, git_end):
    # Clean working directory
    if os.path.isdir(temp_dir):
        shutil.rmtree(temp_dir)
    os.makedirs(temp_dir)
    # Create Meta file
    meta_path = os.path.join(temp_dir, META_FILENAME)
    with open(meta_path, 'w') as outfile:
        yaml.dump({'n': 0, 'p_0': {META_TYPE: generate_type_source}}, outfile)
    # Copy the source program into the temp dir
    program_path = os.path.join(temp_dir, 'p.c' if not enable_git else 'p.sh')
    shutil.copy2(source_path, program_path)
    program_0_path = os.path.join(temp_dir, 'p_0.c')
    shutil.copy2(source_path, program_0_path)

    index = 0
    if enable_mutations:
        index = generate_mutations(program_path, clang_tidy_path, meta_path, mutations)

    if enable_ml:
        index = generate_ml(program_path, apikey_path, meta_path, ml_count, ml_select, ml_interesting, ml_16k)

    if enable_git:
        index = generate_git(goblint_path, temp_dir, meta_path, program_path, git_start, git_end)

    # Add checks with comments
    print(SEPERATOR)
    if enable_git:
        print('Generating goblint checks. This may take a while...')
    for i in range(index + 1):
        print(f"\r[{i}/{index}] Generating goblint checks...", end='')
        sys.stdout.flush()
        file_path = os.path.join(temp_dir, f"p_{i}.c")
        compiling = add_check(file_path, i, goblint_path, meta_path)
        if not compiling:
            continue
        file_path = os.path.join(temp_dir, f"p_{i}_check.c")
        if i == 0 or enable_git:
            add_check_comments(file_path, unknown_instead_of_success=True)
        add_check_comments(file_path, unknown_instead_of_success=False)
    print(f"\r{COLOR_GREEN}Generating goblint checks [DONE]{SPACE}{COLOR_RESET}")

    # Check how many and which files were not compiling
    print(SEPERATOR)
    print("Check if the files compiled...", end='')
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    failed_count = 0
    failed_compilation_keys = []
    for key, value in yaml_data.items():
        if isinstance(value, dict) and META_COMPILING in value and value[META_COMPILING] is False:
            failed_count += 1
            failed_compilation_keys.append(key)
    if failed_count == 0:
        print(f"\r{COLOR_GREEN}All files compiled succesfully{COLOR_RESET}")
    else:
        print(f"\r{COLOR_RED}There were {failed_count} files not compiling (stderr written to {temp_dir}/meta.yaml):{COLOR_RESET} {failed_compilation_keys}")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate programs in the working directory')
    parser.add_argument('source_path', help='Path to the original program or git sh file provided by the user')
    parser.add_argument('temp_dir', help='Path to the working directory')
    parser.add_argument('clang_tidy_path', help='Path to the modified clang-tidy executable')
    parser.add_argument('goblint_path', help='Path to the goblint executable')
    parser.add_argument('--apikey-path', help='Path to the API')
    parser.add_argument('--enable-mutations', action='store_true', help='Enable Mutations. When no mutation is selected all are activated.')
    parser.add_argument('--enable-ml', action='store_true', help='Enable ML')
    parser.add_argument('--enable-git', action='store_true', help='Enable Git')
    parser.add_argument('--ml-count', type=int, default=DEFAULT_ML_COUNT, help='Number of ML programs to generate')
    parser.add_argument('--ml-select', type=int, default=DEFAULT_ML_SELECT, help='Number of selected lines for ML')
    parser.add_argument('--ml-interesting', default="[]", help='Lines to randomly choose the start line for selection (Defaul are all lines)')
    parser.add_argument('--ml-16k', action='store_true', help='Use the 16k mode for ml')
    parser.add_argument('--git-start', help='The starting commit hash for git generation')
    parser.add_argument('--git-end', help='The ending commit hash for git generation')

    # Add mutation options
    add_mutation_options(parser)
    
    args = parser.parse_args()

    # At least one generator has to be enabled
    if not args.enable_mutations and not args.enable_ml and not args.enable_git:
        parser.error("At least one generator has to be enabled (--enable_mutations, --enable-ml, --enable-git)")

    # If using git, only git can be used
    if args.enable_git and (args.enable_ml or args.enable_mutations):
        parser.error("--enable-git cannot be used with --enable-ml or --enable-mutations")

    # If all mutation options are false, set all to true
    mutations = get_mutations_from_args(args)
    non_str_attributes = [attr for attr in vars(mutations) if not attr.endswith('_s')]
    if all(getattr(mutations, attr) is False for attr in non_str_attributes):
        mutations = Mutations(True, True, True, True, True, True)

    # Check required parameters for optional features
    if args.enable_ml and not args.apikey_path:
        parser.error("--enable-ml requires --apikey-path")

    # Check ml intersting string
    if args.ml_interesting != "[]" and validate_interesting_lines(args.ml_interesting, None) == None:
        sys.exit(-1)

    # Check git commit hashes
    git_start_commit = args.git_start
    git_end_commit = args.git_end
    if (git_start_commit == None and git_end_commit != None) or (git_start_commit != None and git_end_commit == None):
        parser.error('[ERROR] Give a git start commit hash AND a end commit hash')

    generate_programs(args.source_path, args.temp_dir, args.clang_tidy_path, args.goblint_path, args.apikey_path, mutations, args.enable_mutations, args.enable_ml, args.enable_git, args.ml_count, args.ml_select, args.ml_interesting, args.ml_16k, args.git_start, args.git_end)

