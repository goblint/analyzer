# Adds "//UNDEFINED" or "//SUCCESS" to the Goblint checks __goblint_check(exp);
# Run with ´python3 script.py example.txt -u´ or ´python3 script.py example.txt -s´

import sys
import re

def process_file(file_name, option):
    if option == '-u':
        print("Add \"//UNDEFINED\" to the Goblint checks __goblint_check(exp);")
    elif option == '-s':
        print("Add \"//SUCCESS\" to the Goblint checks __goblint_check(exp);")

    try:
        with open(file_name, 'r+') as file:
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
            
            file.seek(0)
            file.writelines(modified_lines)
            file.truncate()
            print("Processing complete. Modified lines have been added.")
    except FileNotFoundError:
        print("Error: File not found.")
    except:
        print("An error occurred while processing the file.")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Error: Invalid number of arguments. Provide first a file and then -u for \"//UNDEFINED\" or -s for \"//SUCCESS\".")
    else:
        file_name = sys.argv[1]
        option = sys.argv[2]
        if option not in ['-u', '-s']:
            print("Error: Invalid option. Provide -u for \"//UNDEFINED\" or -s for \"//SUCCESS\".")
        else:
            process_file(file_name, option)
