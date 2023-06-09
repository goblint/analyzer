import argparse
import json
import os
import re
import shutil
import subprocess
import yaml
import sys
sys.path.insert(0, "..")
from util.util import *

def generate_tests(temp_dir, target_dir, precision_test):
    # Check the name of the target_dir
    directoryName = os.path.basename(target_dir)
    pattern = r"\d{2}-\w+"
    if not re.match(pattern, directoryName):
        print(f"{COLOR_RED}[ERROR] Target Directory name is not of the format 01-Name (\d{{2}}-\w+){COLOR_RESET}")
        return

    # Clear and create target_dir
    if os.path.exists(target_dir):
        shutil.rmtree(target_dir)
    os.mkdir(target_dir)

    # Read the meta.yaml
    meta_path = os.path.join(temp_dir,META_FILENAME)
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    n = yaml_data[META_N]

    unchanged_count = 0
    for i in range(n + 1):
        current_program_id = f'p_{i}'
        compilation_success = yaml_data[current_program_id][META_COMPILING]
        if not compilation_success:
            print(f"Generating test files [{i}/{n}] {COLOR_YELLOW}Skipped {i} (Not compiling){COLOR_RESET}")
            continue
        if META_EXCEPTION in yaml_data[current_program_id]:
            print(f"Generating test files [{i}/{n}] {COLOR_YELLOW}Skipped {i} (Exception occured during generation){COLOR_RESET}")
            continue
        type = yaml_data[current_program_id][META_TYPE]
        if type == Generate_Type.SOURCE.value or (type == Generate_Type.GIT.value and i == 0):
            continue
        if (i-1) % 9 == 0:
            print(f"Generating test files [{i}/{n}]")
        sub_type = yaml_data[current_program_id][META_SUB_TYPE]
        if type == Generate_Type.MUTATION.value or type == Generate_Type.GIT.value:
            test_name = _format_number(i) + '-' + type + '_' + sub_type + '_' + _format_number(i)
        else:
            test_name = _format_number(i) + '-' + type + '_' + _format_number(i)

        # Select depending on generator the start and end file of at test
        if type == Generate_Type.MUTATION.value or type == Generate_Type.ML.value:
            source_program_id = 'p_0'
            start_program = os.path.join(temp_dir, current_program_id + '_check_success.c')
            end_program = os.path.join(temp_dir, source_program_id + '_check_unknown.c')
            end_program_precison = os.path.join(temp_dir, source_program_id + '_check_success.c')   
        elif type ==Generate_Type.GIT.value:
            previous_program_id = f'p_{i-1}'

            start_program = os.path.join(temp_dir, previous_program_id + '_check_success.c')
            end_program = os.path.join(temp_dir, current_program_id + '_check_unknown.c')
            end_program_precison = os.path.join(temp_dir, current_program_id + '_check_success.c')
        else:
            print(f'{COLOR_RED}[ERROR] Trying to generate tests from unknown generator type{COLOR_RESET}')
            sys.exit(-1)
        
        # Copy mutated code as the original code
        shutil.copy2(start_program, os.path.join(target_dir, test_name + '.c'))
        # Create a patch file
        patch_path = os.path.join(target_dir, test_name + '.patch')
        command = 'diff -u {} {} > {}'.format(
            os.path.join(target_dir, test_name + '.c'),
            end_program_precison if precision_test else end_program,
            patch_path
        )
        result = subprocess.run(command, shell=True)
        _fix_patch_file(patch_path, directoryName, test_name + '.c')
        if result.returncode in [0, 1]:
            if result.returncode == 0:
                print(f"{COLOR_YELLOW}[WARNING] There were no changes in the patch for test {i}{COLOR_RESET}")
                unchanged_count += 1
                yaml_data[current_program_id][META_DIFF] = False
            else:
                yaml_data[current_program_id][META_DIFF] = True
        else:
            raise Exception("Command failed with return code: {}".format(result.returncode))
        # Create a empty config file
        #TODO Support other config files
        data = {}
        with open(os.path.join(target_dir, test_name + '.json'), 'w') as f:
            json.dump(data, f)
    print(f"{COLOR_GREEN}Generating test files [DONE].{COLOR_RESET}")
    if unchanged_count > 0:
        print(f'{COLOR_YELLOW}There were {unchanged_count} patch files with no changes.{COLOR_RESET}')

    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)

def _fix_patch_file(patch_file, folder_name, file_name):
    with open(patch_file, 'r') as file:
        lines = file.readlines()

    with open(patch_file, 'w') as file:
        for line in lines:
            if line.startswith('---') or line.startswith('+++'):
                line = line.split(' ')[0] + " " + "tests/incremental/" + folder_name + "/" + file_name + "\n"
            file.write(line)

def _format_number(n):
    return str(n).zfill(2)
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate Test files in the target directory based on the working directory')
    parser.add_argument('temp_dir', help='Path to the working directory')
    parser.add_argument('target_dir', help='Path to the target directory')
    parser.add_argument('-p', '--precision-test', action='store_true', help='Generate tests for precision')

    args = parser.parse_args()

    generate_tests(args.temp_dir, args.target_dir, args.precision_test)
