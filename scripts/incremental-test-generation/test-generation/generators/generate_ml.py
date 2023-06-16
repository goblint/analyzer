import argparse
import ast
import os
import random
import sys
import time
import openai
import yaml
from concurrent.futures import ThreadPoolExecutor
from multiprocessing import Lock

sys.path.insert(0, "..")
from util.util import *

SEPERATOR_EXPLANATION_START = 'EXPLANATION>'
SEPERATOR_EXPLANATION_END = '<EXPLANATION_END'
SEPERATOR_CODE_START = 'CODE>'
SEPERATOR_CODE_END = '<CODE_END'

error_counter = 0

def generate_ml(program_path, apikey_path, meta_path, ml_count, num_selected_lines, interesting_lines, ml_16k):
    with open(meta_path, 'r') as file:
        yaml_data = yaml.safe_load(file)
        index: int = yaml_data[META_N]
    yaml_data[META_N] = index + ml_count
    with open(meta_path, 'w') as file:
        yaml.safe_dump(yaml_data, file)
    
    # Read the api key and organisation
    with open(apikey_path, 'r') as file:
        data = yaml.safe_load(file)
    organisation = data.get('organisation')
    api_key = data.get('api-key')

    # Authenticate
    openai.organization = organisation
    openai.api_key = api_key
    
    # Get interesting lines
    with open(program_path, "r") as file:
        max_line = len(file.readlines())
    if max_line < num_selected_lines:
        num_selected_lines = max_line
    interesting_lines = validate_interesting_lines(interesting_lines, program_path)
    interesting_lines = _reformat_interesting_lines(num_selected_lines, interesting_lines, max_line)
    
    print(SEPERATOR)
    interesting_lines_string = 'Start lines are randomly choosen from all lines.' if interesting_lines == [] else f' Start lines are randomly choosen from {interesting_lines}.'
    print(f'[ML] Start making {ml_count} requests with ML. {ML_WORKERS} are executed in parallel. {num_selected_lines} from {max_line} lines will be selected. {interesting_lines_string}')
    file_lock = Lock()
    with ThreadPoolExecutor(max_workers=ML_WORKERS) as executor:
        for i in range(ml_count):
            executor.submit(_iterative_mutation_generation, program_path, meta_path, interesting_lines, ml_16k, num_selected_lines, max_line, index + i + 1, file_lock)

    print(SEPERATOR)
    print('Check if all ML requests finsihed succesfully...')
    print(f'{COLOR_GREEN}Finished all requests.{COLOR_RESET}' + (f'{COLOR_RED} {error_counter} failed with an exception.{COLOR_RESET}' if error_counter > 0 else ''))

    return index + ml_count

def _iterative_mutation_generation(program_path, meta_path, interesting_lines, ml_16k, num_selected_lines, max_line, index, lock):
    try:
        time.sleep((index * 50)/1000) # Sleep depending on index to print the start messages in the right order
        new_path = make_program_copy(program_path, index)
        (explanation, selected_lines) = _apply_mutation(new_path, interesting_lines, ml_16k, num_selected_lines, max_line, index)
        _write_meta_data(meta_path, selected_lines, explanation, index, lock)
    except Exception as e:
        print(f"{COLOR_RED}[{index}] Error for request {index}:{COLOR_RESET} {e}")
        _write_meta_data(meta_path, [], '', index, lock, exception=e)
    return index

def _apply_mutation(new_path, interesting_lines, ml_16k, num_selected_lines, max_line, index):
    # Get the original lines
    with open(new_path, "r") as file:
        lines = file.readlines()

    # Get code snippet
    selected_lines = _select_lines(interesting_lines, num_selected_lines, max_line)
    snippet = ''
    for i in selected_lines:
        snippet += lines[i]

    print(f"[{index}][{Generate_Type.ML.value}][REQUEST] Make request for lines [{selected_lines.start}, {selected_lines.stop}]. This may take a few seconds...")

    # Get response from gpt
    response = _make_gpt_request(snippet, ml_16k)

    # Extract Explanation
    explanation_start = response.find(SEPERATOR_EXPLANATION_START) + len(SEPERATOR_EXPLANATION_START)
    explanation_end = response.find(SEPERATOR_EXPLANATION_END)
    explanation = response[explanation_start:explanation_end].strip()

    # Extract Code lines
    code_start = response.find(SEPERATOR_CODE_START) + len(SEPERATOR_CODE_START)
    code_end = response.find(SEPERATOR_CODE_END)
    code = response[code_start:code_end].strip()
    new_code_lines = code.splitlines()
    new_code_lines = [line for line in new_code_lines if line != '```' and line != '```c' and line != '``']

    # Comment out the original lines
    for i in selected_lines:
        lines[i] = '// ' + lines[i]

    # Add start marker
    lines.insert(selected_lines[0], f'//[ML][START] {explanation.replace("/n", "")}\n')

    # Insert new lines of code
    for i, new_line in enumerate(new_code_lines, start=selected_lines[0] + 1):
        lines.insert(i, new_line + '\n')

    # Add end marker
    lines.insert(selected_lines[0] + len(new_code_lines) + 1, '//[ML][END]\n')

    # Write the updated lines back to the file
    with open(new_path, "w") as file:
        file.writelines(lines)

    explanation_lines = explanation.splitlines()
    limited_explanation = "\n".join(explanation_lines[:4])
    print(f'{COLOR_GREEN}[{index}] Finished request:{COLOR_RESET} {limited_explanation}')

    return (explanation, selected_lines)
    

def _write_meta_data(meta_path, selected_lines, explanation, index, lock, exception=None):
    lock.acquire()
    global error_counter
    try:
        with open(meta_path, 'r') as file:
            yaml_data = yaml.safe_load(file)
        if exception == None:
            yaml_data[f"p_{index}"] = {
                META_TYPE: Generate_Type.ML.value,
                META_SUB_TYPE: explanation,
                META_LINES: f'[{selected_lines.start}, {selected_lines.stop}]'
            }
        else:
            error_counter += 1
            yaml_data[f"p_{index}"] = {
                META_TYPE: Generate_Type.ML.value,
                META_EXCEPTION: str(exception)
            }
        with open(meta_path, 'w') as file:
            yaml.safe_dump(yaml_data, file)
    finally:
        lock.release()

def _make_gpt_request(snippet, ml_16k):
    prompt = f'''
        You are a developer for C helping me with my following question. I want to understand the typical process of code evolution by looking at how developers make changes over time for testing an incremental analysis of the static c analyzer Goblint.

        The following mutations are already generated by me. So please do not generate programs that can be generated by this mutations: Removal of function bodies, Inversion of if statements, Switching <= with < and >= with >, Replacing constants unequal 0 with 1, Replace pthread calls with function calls, Switching && with ||. Please do not consider these mutations as examples how your code changes should look like. Just try to prevent doing things that could be done with these mutations.

        Below is an snippet from a C file which represents a part of the finished program. My question is how a previous version of this code could have looked like before some typical code changes done by developers. Can you generate me such a previous version?

        The code you generate should be a self-contained snippet that could directly replace the provided excerpt in the original, complete program. It should preserve the overall functionality of the program and must not cause any compilation errors when reintegrated into the larger code base. Please consider the dependencies and interactions with other parts of the program when generating the previous version of the code. Your generated code should be able to interact correctly with the rest of the program just like the original excerpt does. You do not have to add import statements or function declarations or closing brackets when these are cut off in the snippet, but when they are in the snippet you need to add them to preserve the whole program.

        Use these keywords (\"{SEPERATOR_EXPLANATION_START}\", \"{SEPERATOR_EXPLANATION_END}\", \"{SEPERATOR_CODE_START}\", \"{SEPERATOR_CODE_END}\") to structure you answer. You answer should have the following structure for better identifying the different parts of the response: {SEPERATOR_EXPLANATION_START} (Explain what you have changed in one or two sentences) {SEPERATOR_EXPLANATION_END} {SEPERATOR_CODE_START} (the previous version of the code) {SEPERATOR_CODE_END}

        ```c
            {snippet}
        ```
        '''

    if ml_16k:
        model = "gpt-3.5-turbo-16k"
    else:
        model = "gpt-3.5-turbo"

    response = openai.ChatCompletion.create(
        model=model,
        n = 1,
        messages=[
            {"role": "user", "content": prompt},
        ]
    ).choices[0].message['content']

    return response

def _reformat_interesting_lines(num_selected_lines, interesting_lines, max_line):
    for i in range(len(interesting_lines)):
        interesting_lines[i] = int(interesting_lines[i])
        # Adjust for line array starting with 0 but in input first line is 1
        interesting_lines[i] -= 1
        # When line + num_selected_lines is greater then max_line correct the line downwards
        if interesting_lines[i] + num_selected_lines > (max_line):
            interesting_lines[i] =  (max_line) - num_selected_lines
    return interesting_lines

def _select_lines(interesting_lines, num_selected_lines, max_line):
    if interesting_lines == []:
        selected_line = random.randint(0, (max_line) - num_selected_lines)
    else:
        selected_line = random.choice(interesting_lines)
    return range(selected_line, selected_line + num_selected_lines)

def validate_interesting_lines(intersting_lines_string: str, program_path):
    if program_path != None:
        with open(program_path, "r") as file:
            max_line = len(file.readlines())
    else:
        max_line = None

    try:
        intersting_lines = ast.literal_eval(intersting_lines_string)
    except SyntaxError:
        print(f"{COLOR_RED}The format \"{intersting_lines_string}\" is incorrect! Please use a format like this: \"[1, 42, 99]\"{COLOR_RESET}")
        return None
    except Exception as e:
        print(f"{COLOR_RED}An unexpected error occurred reading the input \"{intersting_lines_string}\":{COLOR_RESET} {e}")
        return None
    
    if max_line != None:
        for line in intersting_lines:
            if line <= 0:
                print(f"{COLOR_RED}All lines in \"{intersting_lines_string}\" should be greater zero!{COLOR_RESET}")
                return None
            if line > max_line:
                print(f"{COLOR_RED}All lines in \"{intersting_lines_string}\" should be below the maximum line {max_line}!{COLOR_RESET}")
                return None
        
    return intersting_lines

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate mutations with ML.")
    parser.add_argument("program", help="Path to the C program")
    parser.add_argument("apikey", help="Path to the api key")
    parser.add_argument("meta_path", help="Path to the meta_file")
    parser.add_argument("ml_count", help="How many different programs should be generated with ML")
    parser.add_argument("num_selected_lines", help="How many lines to consider")
    parser.add_argument("interesting_lines", help="Which parts are interesting (All: [], Specify: \"[1, 42, 99]\")")
    parser.add_argument('-m16', '--model-16k', action='store_true', help='Run with the 16k model instead of the 4k')

    args = parser.parse_args()

    interesting_lines = validate_interesting_lines(args.interesting_lines, args.program)
    if interesting_lines == None:
        print(f'{COLOR_RED}Stopped program execution{COLOR_RESET}')
        sys.exit(-1)

    generate_ml(args.program, args.apikey, args.meta_path, int(args.ml_count), int(args.num_selected_lines), interesting_lines, args.model_16k)