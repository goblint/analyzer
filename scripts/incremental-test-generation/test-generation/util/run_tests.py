import argparse
import os
import shutil
import subprocess
import sys
sys.path.insert(0, "..")
from util.util import *

def run_tests(test_dir, goblint_repo_dir, cfg):
    # Check the name of the test_dir
    test_dir_name = os.path.basename(test_dir)
    if test_dir_name != "99-temp":
        print(f"{COLOR_RED}[ERROR] The test directory name has to be \'99-temp\'{COLOR_RESET}")

    incremental_tests_dir_abs = os.path.abspath(os.path.join(goblint_repo_dir, "tests", "incremental", test_dir_name))
    if os.path.exists(incremental_tests_dir_abs):
        shutil.rmtree(incremental_tests_dir_abs)
    shutil.copytree(test_dir, incremental_tests_dir_abs)

    ruby_path_abs = os.path.abspath(os.path.join(goblint_repo_dir, "scripts", "update_suite.rb"))
    original_dir = os.getcwd()
    os.chdir(goblint_repo_dir)
    command = f"{ruby_path_abs} group temp -i"
    if cfg:
        command += " -c"
    process = subprocess.Popen(command, stdout=subprocess.PIPE, universal_newlines=True, shell=True)
    for line in process.stdout:
        print(line, end='')
    process.wait()

    shutil.rmtree(incremental_tests_dir_abs)
    os.chdir(original_dir)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run the tests in the specified test directory with the ruby script from Goblint')
    parser.add_argument('test_dir', help='Path to the directory with the tests')
    parser.add_argument('goblint_repo_dir', help='Path to the Goblint repository')
    parser.add_argument('-c', '--cfg', action='store_true', help='Run with fine-grained cfg-based change detection')

    args = parser.parse_args()

    run_tests(args.test_dir, args.goblint_repo_dir, args.cfg)