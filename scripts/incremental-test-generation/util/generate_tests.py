import argparse
import json
import os
import re
import shutil
import subprocess
import questionary
import yaml
import sys
sys.path.insert(0, "..")
from util.util import *

def generate_tests(temp_dir, target_dir, goblint_config, precision_test, temp_name):
    # Check the name of the target_dir
    directory_name = os.path.basename(target_dir)
    if not temp_name and not check_test_name(directory_name):
        sys.exit(-1)

    if os.path.exists(target_dir):
        print(f'{COLOR_RED}The test directory {target_dir} already exists.{COLOR_RESET}')
        if questionary.confirm('Replace the directory?', default=True).ask():
            shutil.rmtree(target_dir)
        else:
            sys.exit(-1)
    os.makedirs(target_dir)

    # Read the meta.yaml
    meta_path = os.path.join(temp_dir,META_FILENAME)
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
    n = yaml_data[META_N]

    # loop threw all generated mutations
    original_target_dir = target_dir
    test_paths = [target_dir]
    current_test_num = 1 # Skip num 0 to align program index with test number
    current_dir_num = int(directory_name[:2])
    unchanged_count = 0
    compiling_programs = []
    if temp_name and int(directory_name[:3]) != 100:
        print(f'{COLOR_RED}[ERROR] The directory number for temp files must be 100 but was {directory_name}{COLOR_RESET}')
        sys.exit(-1)
    elif temp_name:
        current_dir_num = 100
        original_target_dir = os.path.join(os.path.dirname(target_dir), '99' + os.path.basename(target_dir)[3:])
    for i in range(n + 1):
        if current_test_num > 99:
            current_dir_num += 1

            # When temporary files let the files go over 99 to rename them later
            if not temp_name and current_dir_num > 99:
                print(f'{COLOR_RED}[ERROR] The directory number 100 is out of range. Consider starting with a lower than {directory_name} {COLOR_RESET}')
                sys.exit(-1)

            group_name = re.match(r'\d+-(.*)', directory_name).group(1)
            target_dir = os.path.join(os.path.dirname(target_dir), f'{current_dir_num:02}-{group_name}')
            test_paths.append(target_dir)

            if os.path.exists(target_dir):
                print(f'{COLOR_RED}The test directory {target_dir} already exists.{COLOR_RESET}')
                if questionary.confirm('Replace the directory?', default=True).ask():
                    shutil.rmtree(target_dir)
                else:
                    sys.exit(-1)
            os.mkdir(target_dir)

            current_test_num = 0

        current_program_id = f'p_{i}'
        compilation_success = yaml_data[current_program_id][META_COMPILING]
        if compilation_success:
            compiling_programs.append(i)
        else:
            print(f"\rGenerating test files [{i}/{n}] {COLOR_YELLOW}Skipped {i} (Not compiling){COLOR_RESET}")
            continue
        if META_EXCEPTION in yaml_data[current_program_id]:
            print(f"\rGenerating test files [{i}/{n}] {COLOR_YELLOW}Skipped {i} (Exception occured during generation){COLOR_RESET}")
            continue
        type = yaml_data[current_program_id][META_TYPE]
        if type == Generate_Type.SOURCE.value or (type == Generate_Type.GIT.value and i == 0):
            continue
        if (i-1) % 9 == 0:
            print(f"\rGenerating test files [{i}/{n}]", end='')
        sub_type = yaml_data[current_program_id][META_SUB_TYPE]
        if type == Generate_Type.MUTATION.value or type == Generate_Type.GIT.value:
            test_name = f'{_format_number(current_test_num)}-{type}_{sub_type}_{_format_number(i)}'
        else:
            test_name = f'{_format_number(current_test_num)}-{type}_{_format_number(i)}'

        # Select depending on generator the start and end file of at test
        if type == Generate_Type.MUTATION.value or type == Generate_Type.ML.value:
            source_program_id = 'p_0'
            start_program = os.path.join(temp_dir, current_program_id + '_check_success.c')
            end_program = os.path.join(temp_dir, source_program_id + '_check_unknown.c')
            end_program_precison = os.path.join(temp_dir, source_program_id + '_check_success.c')   
        elif type ==Generate_Type.GIT.value:
            # If it's the first compiling program skip it.
            if i == compiling_programs[0]:
                print(f"\rGenerating test files [{i}/{n}] {COLOR_BLUE}Skipped {i} as the source file for the first test{COLOR_RESET}")
                continue

            # Find the index of the previous compiling program.
            previous_program_index = compiling_programs.index(i) - 1
            previous_program_id = f'p_{compiling_programs[previous_program_index]}'

            start_program = os.path.join(temp_dir, previous_program_id + '_check_success.c')
            end_program = os.path.join(temp_dir, current_program_id + '_check_unknown.c')
            end_program_precison = os.path.join(temp_dir, current_program_id + '_check_success.c')
        else:
            print(f'\n{COLOR_RED}[ERROR] Trying to generate tests from unknown generator type{COLOR_RESET}')
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
        if temp_name:
            # For patch files keep the 99 for running inplace after renaming folder
            _fix_patch_file(patch_path, os.path.basename(original_target_dir), test_name + '.c')
        else:
            _fix_patch_file(patch_path, os.path.basename(target_dir), test_name + '.c')
        if result.returncode in [0, 1]:
            if result.returncode == 0:
                print(f"\r{COLOR_YELLOW}[WARNING] There were no changes in the patch for test {i}{COLOR_RESET}")
                unchanged_count += 1
                yaml_data[current_program_id][META_DIFF] = False
            else:
                yaml_data[current_program_id][META_DIFF] = True
        else:
            raise Exception("Command failed with return code: {}".format(result.returncode))
        if goblint_config == None:
            # Create a empty config file
            data = {}
            with open(os.path.join(target_dir, test_name + '.json'), 'w') as f:
                json.dump(data, f)
        else:
            # Copy config file
            shutil.copy2(os.path.abspath(os.path.expanduser(goblint_config)), os.path.join(target_dir, test_name + '.json'))

        current_test_num += 1

    print(f"\r{COLOR_GREEN}Generating test files [DONE]{SPACE}{COLOR_RESET}")
    if unchanged_count > 0:
        print(f'{COLOR_YELLOW}There were {unchanged_count} patch files with no changes.{COLOR_RESET}')

    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)

    # Return the generated directories
    return test_paths

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
    parser.add_argument('-c', '--goblint-config', help='Optional path to the goblint config file used for the tests (using no option creates an empty one)')
    parser.add_argument('-t', '--temp-name', action='store_true', help='Store name in special format for running the tests and removing them directly again')

    args = parser.parse_args()

    generate_tests(args.temp_dir, args.target_dir, args.goblint_config, args.precision_test, args.temp_name)
