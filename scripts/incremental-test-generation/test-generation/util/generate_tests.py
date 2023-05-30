import argparse
import json
import os
import re
import shutil
import subprocess
import yaml
import sys
import datetime
sys.path.append("..")
from util import *

def generate_tests(temp_dir, target_dir, precision_test):
    # Check the name of the target_dir
    directoryName = os.path.basename(target_dir)
    pattern = r"\d{2}-\w+"
    if not re.match(pattern, directoryName):
        print("[ERROR] Target Directory name is not of the format 01-Name (\d{2}-\w+)")
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

    source_program_id = 'p_0'
    source_program = os.path.join(temp_dir, source_program_id + '_c_u.c')
    source_program_precison = os.path.join(temp_dir, source_program_id + '_c.c')
    unchanged_count = 0
    for i in range(n + 1):
        generated_id = 'p_' + str(i)
        generated_program = os.path.join(temp_dir, generated_id + '_c.c')
        compilation_success = yaml_data[generated_id][META_COMPILING]
        if not compilation_success:
            print(f"Generating test files [{i}/{n}] Skipped {i} (Not compiling)")
            continue
        type = yaml_data[generated_id][META_TYPE]
        if type == Generate_Type.SOURCE.value:
            continue
        if (i-1) % 9 == 0:
            print(f"Generating test files [{i}/{n}]")
        if type == Generate_Type.MUTATION.value:
            sub_type = yaml_data[generated_id][META_SUB_TYPE]
            test_name = _format_number(i-1) + '-' + type + '_' + sub_type
            # Copy mutated code as the original code
            shutil.copy2(generated_program, os.path.join(target_dir, test_name + '.c'))
            # Create a patch file
            patch_path = os.path.join(target_dir, test_name + '.patch')
            command = 'diff -u {} {} > {}'.format(
                os.path.join(target_dir, test_name + '.c'),
                source_program_precison if precision_test else source_program,
                patch_path
            )
            result = subprocess.run(command, shell=True)
            _fix_patch_file(patch_path, directoryName, test_name + '.c')
            if result.returncode in [0, 1]:
                if result.returncode == 0:
                    print(f"[WARNING] There were no changes in the patch for test {i}")
                    unchanged_count += 1
                    yaml_data[generated_id][META_DIFF] = False
                else:
                    yaml_data[generated_id][META_DIFF] = True
            else:
                raise Exception("Command failed with return code: {}".format(result.returncode))
            # Create a empty config file
            #TODO Support other config files
            data = {}
            with open(os.path.join(target_dir, test_name + '.json'), 'w') as f:
                json.dump(data, f)
        if type == Generate_Type.ML.value:
            #TODO
            print ('[ERROR] Generating ML Tests not implemented')
        if type == Generate_Type.GIT.value:
            #TODO
            print ('[ERROR] Generating GIT Tests not implemented')
    print(f"Generating test files [DONE]")

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
