import argparse
import os
import shutil
import sys
sys.path.append("..")
from generators.generate_mutations import *
from util.add_check import *
from util.add_check_comments import *

generate_type_source = "SOURCE"

def gernerate_programs(source_path, temp_dir, clang_tidy_path, goblint_path, apikey_path, git_url, mutations, enable_mutations, enable_ml,  enable_git):
    # Clean working directory
    if os.path.isdir(temp_dir):
        shutil.rmtree(temp_dir)
    os.makedirs(temp_dir)
    # Create Meta file
    meta_path = os.path.join(temp_dir,'meta.yaml')
    with open(meta_path, 'w') as outfile:
        yaml.dump({'n': 0, 'p_0': {'type': generate_type_source}}, outfile)
    # Copy the source program into the temp dir
    program_path = os.path.join(temp_dir, 'p.c')
    shutil.copy2(source_path, program_path)
    program_0_path = os.path.join(temp_dir, 'p_0.c')
    shutil.copy2(source_path, program_0_path)

    index = 0
    if enable_mutations:
        index = generate_mutations(program_path, clang_tidy_path, meta_path, mutations)

    if enable_ml:
        pass

    if enable_git:
        pass

    # Add checks with comments
    print(seperator)
    index += 1
    for i in range(index):
        if i % 9 == 0:
            print(f"Generating goblint checks [{i+1}/{index}]")
        file_path = os.path.join(temp_dir, f"p_{i}.c")
        add_check(file_path, i, goblint_path, meta_path)
        file_path = os.path.join(temp_dir, f"p_{i}_c.c")
        if i == 0:
            add_check_comments(file_path, True)
        add_check_comments(file_path, False)
    print(f"Generating goblint checks [DONE]")

    # Check how many and which files were not compiling
    print(seperator)
    print("Check if the files compiled...")
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    failed_count = 0
    failed_compilation_keys = []
    for key, value in yaml_data.items():
        if isinstance(value, dict) and 'compilation' in value and value['compilation'] is False:
            failed_count += 1
            failed_compilation_keys.append(key)
    if failed_count == 0:
        print("All files compiled succesfully")
    else:
        print(f"There where {failed_count} files not compiling: {failed_compilation_keys}")




if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate programs in the working directory')
    parser.add_argument('source_path', help='Path to the original program provided by the user')
    parser.add_argument('temp_dir', help='Path to the working directory')
    parser.add_argument('clang_tidy_path', help='Path to the modified clang-tidy executable')
    parser.add_argument('goblint_path', help='Path to the goblint executable')
    parser.add_argument('--apikey-path', help='Path to the API')
    parser.add_argument('--git-url', help='Git URL')
    parser.add_argument('--enable-mutations', action='store_true', help='Enable Mutations. When no mutation is selected all are activated.')
    parser.add_argument('--enable-ml', action='store_true', help='Enable ML')
    parser.add_argument('--enable-git', action='store_true', help='Enable Git')

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

    if args.enable_git and not args.git_url:
        parser.error("--enable-git requires --git-url")

    gernerate_programs(args.source_path, args.temp_dir, args.clang_tidy_path, args.goblint_path, args.apikey_path, args.git_url, mutations, args.enable_mutations, args.enable_ml, args.enable_git)
