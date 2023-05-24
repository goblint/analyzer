# Adds "//UNDEFINED" or "//SUCCESS" to the Goblint checks "__goblint_check(exp);".
# Stores the file with the same name when adding "//SUCCESS".
# Stores the file with the appendix _u when adding "//UNDEFINED".
# Run with ´python3 script.py example.txt -u´ or ´python3 script.py example.txt -s´.

import argparse
import sys
import re

def process_file(file_name, option):
    if option == 'u':
        print("Add \"//UNDEFINED\" to the Goblint checks __goblint_check(exp);")
    elif option == 's':
        print("Add \"//SUCCESS\" to the Goblint checks __goblint_check(exp);")

    try:
        with open(file_name, 'r') as file:
            lines = file.readlines()
            modified_lines = []
            
            for line in lines:
                if '__goblint_check(' in line:
                    match = re.search(r'(__goblint_check\(.*?\);)', line)
                    if match:
                        modified_line = match.group(1)
                        if option == '-u':
                            modified_line += ' //UNDEFINED'
                        elif option == '-s':
                            modified_line += ' //SUCCESS'
                        line = line.replace(match.group(1), modified_line)
                modified_lines.append(line)
            
            if option == '-u':
                new_file_name = file_name.rsplit('.', 1)[0] + '_u.c'
                with open(new_file_name, 'w') as new_file:
                    new_file.writelines(modified_lines)
                print("Processing complete. Modified lines have been added to the new file:", new_file_name)
            elif option == '-s':
                with open(file_name, 'w') as file:
                    file.writelines(modified_lines)
                print("Processing complete. Modified lines have been added to the existing file:", file_name)
    except FileNotFoundError:
        print("Error: File not found.")
    except:
        print("An error occurred while processing the file.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="file name")
    parser.add_argument("-u", "--undefined", action="store_true", help="option for //UNDEFINED")
    parser.add_argument("-s", "--success", action="store_true", help="option for //SUCCESS")
    args = parser.parse_args()

    if not (args.undefined or args.success):
        parser.error("Error: Invalid option. Provide -u for \"//UNDEFINED\" or -s for \"//SUCCESS\".")

    process_file(args.file, "u" if args.undefined else "s")
