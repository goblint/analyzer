import argparse
import os
import shutil
import subprocess
import re
import sys

import questionary
sys.path.insert(0, "..")
from util.util import *

def run_tests(program_path, test_dir, goblint_repo_dir, cfg):
    # When the directory has a starting number >99 rename it for in place running of the tests
    match = re.match(r'(\d+)-(.*)', os.path.basename(test_dir))
    if match:
        number, text = match.groups()
        number = int(number)
        if number > 99:
            number = 99
        new_name = f'{number}-{text}'
        os.rename(test_dir, os.path.join(os.path.dirname(test_dir), new_name))
        test_dir = os.path.join(os.path.dirname(test_dir), new_name)
    else:
        print(f"{COLOR_RED}[ERROR] The test directory had not the format number-text{COLOR_RESET}")


    # Check the name of the test_dir
    test_dir_name = os.path.basename(test_dir)
    if test_dir_name != "99-temp":
        print(f"{COLOR_RED}[ERROR] The test directory name has to be \'99-temp\'{COLOR_RESET}")
        sys.exit(-1)

    incremental_tests_dir_abs = os.path.abspath(os.path.join(goblint_repo_dir, "tests", "incremental", test_dir_name))
    if os.path.exists(incremental_tests_dir_abs):
        print(f'{COLOR_RED}The test directory {incremental_tests_dir_abs} already exists.{COLOR_RESET}')
        if questionary.confirm('Replace the directory?', default=True).ask():
            shutil.rmtree(incremental_tests_dir_abs)
        else:
            sys.exit(-1)
    shutil.copytree(test_dir, incremental_tests_dir_abs)

    ruby_path_abs = os.path.abspath(os.path.join(goblint_repo_dir, "scripts", "update_suite.rb"))
    params = get_params_from_file(program_path)
    if params != "":
        print(f"\n{COLOR_YELLOW}Using parameters from input file:{COLOR_RESET} {params}")
    original_dir = os.getcwd()
    os.chdir(goblint_repo_dir)
    command = f"{ruby_path_abs} group temp -p \"{params}\" -i"
    if cfg:
        command += " -c"
    process = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)

    # Print the output including carriage returns
    line = ''
    while process.poll() is None:
        char = process.stdout.read(1).decode('utf-8')
        if char == '\r' or char == '\n':
            sys.stdout.write('\r' + line)
            sys.stdout.flush()
            if char == '\n':
                print()
            line = ''
        else:
            line += char
    if line:
        sys.stdout.write('\r' + line + '\n')
        sys.stdout.flush()

    process.wait()

    shutil.rmtree(incremental_tests_dir_abs)
    shutil.rmtree(test_dir)
    os.chdir(original_dir)

def get_params_from_file(filename):
    param_pattern = re.compile(r"\s*//.*PARAM\s*:\s*(.*)")

    with open(filename, 'r') as f:
        for line in f:
            match = param_pattern.match(line)
            if match:
                params = match.group(1).strip()
                return params
                
    return ""

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run the tests in the specified test directory with the ruby script from Goblint')
    parser.add_argument('program_path', help='Path to the input file of the user')
    parser.add_argument('test_dir', help='Path to the directory with the tests (WARNING Will be removed!)')
    parser.add_argument('goblint_repo_dir', help='Path to the Goblint repository')
    parser.add_argument('-c', '--cfg', action='store_true', help='Run with fine-grained cfg-based change detection')

    args = parser.parse_args()

    run_tests(args.program_path, args.test_dir, args.goblint_repo_dir, args.cfg)