# Takes a file and generates the goblint checks
# Stores the file with an additional "_c"
# When there is a compilation error the process writes [META_EXCEPTION] into the meta data file for the given index

import argparse
import subprocess
import sys
import yaml
sys.path.insert(0, "..")
from util.util import *

def add_check(file_path: str, index: int, goblint_path: str, meta_path: str):
    command = [
    goblint_path,
    "--enable",
    "trans.goblint-check",
    "--set",
    "trans.activated",
    '["assert"]',
    "--set",
    "trans.output",
    file_path.rsplit('.', 1)[0] + '_check.c',
    file_path
    ]

    result = subprocess.run(command, text=True, capture_output=True)

    compiling = result.returncode == 0

    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        yaml_data["p_" + str(index)][META_COMPILING] = compiling
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)
    
    if not compiling:
        print(f"{COLOR_RED}Error compiling program with index {index}.{COLOR_RESET}")
        if index == 0 and not yaml_data["p_0"][META_TYPE] == Generate_Type.GIT.value:
            print(f"{COLOR_RED}The original program did not compile. Stopping program!{COLOR_RESET}")
            sys.exit(-1)
        with open(meta_path, 'r') as file:
            yaml_data = yaml.safe_load(file)
        yaml_data[f"p_{index}"] = {
                META_TYPE: Generate_Type.ML.value,
                META_EXCEPTION: result.stderr,
                META_COMPILING: False
            }
        with open(meta_path, 'w') as file:
            yaml.safe_dump(yaml_data, file)
        return False
    else:
        return True

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate __goblint_check() for a C file. When not compiling write [NOT COMPILING] in the meta file')

    # Add the required arguments
    parser.add_argument('file', help='Path to the C file')
    parser.add_argument('index', help='Index of the file (needed for meta data)')
    parser.add_argument('goblint', help='Path to the Goblint executable')
    parser.add_argument('meta', help='Path to the meta data file')

    # Parse the command-line arguments
    args = parser.parse_args()

    # Call the process_file function with the provided arguments
    add_check(args.file, args.index, args.goblint, args.meta)
