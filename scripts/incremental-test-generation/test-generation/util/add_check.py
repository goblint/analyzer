# Takes a file and generates the goblint checks
# Stores the file with an additional "_c"
# When there is a compilation error the process writes [COMPILE_FAIL] into the meta data file for the given index

import argparse
import subprocess
import sys
import yaml
sys.path.append("..")
from util import *

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
    file_path.rsplit('.', 1)[0] + '_c.c',
    file_path
    ]

    result = subprocess.run(command, text=True, capture_output=True)

    compiling = result.returncode == 0

    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        yaml_data["p_" + str(index)][META_COMPILING] = compiling
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)
    
    if not compiling :
        print(result.stdout)
        print(result.stderr)
        print(f"Error compiling program with index {index}.")
        sys.exit(-1)

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
