import argparse
import os
import shutil
import questionary
import yaml
from pathlib import Path
from util.util import *
from util.generate_programs import gernerate_programs
from util.generate_tests import generate_tests
from util.run_tests import run_tests
from generators.generate_mutations import add_mutation_options, get_mutations_from_args

logo = '''Use -h to see the command line options

         __  __       _        _   _                    
        |  \/  |     | |      | | (_)                   
        | \  / |_   _| |_ __ _| |_ _  ___  _ __         
        | |\/| | | | | __/ _` | __| |/ _ \| '_ \        
        | |  | | |_| | || (_| | |_| | (_) | | | |       
        |_|  |_|\__,_|\__\__,_|\__|_|\___/|_| |_|       
          _____                           _             
         / ____|                         | |            
        | |  __  ___ _ __   ___ _ __ __ _| |_ ___  _ __ 
        | | |_ |/ _ \ '_ \ / _ \ '__/ _` | __/ _ \| '__|
        | |__| |  __/ | | |  __/ | | (_| | || (_) | |   
         \_____|\___|_| |_|\___|_|  \__,_|\__\___/|_|   
                                                        
                                                        

        '''

def run(goblint_path, llvm_path, input_path, is_mutation, is_ml, is_git, mutations, create_precision, is_run_tests):
    # Make paths absolute
    goblint_path = os.path.abspath(os.path.expanduser(goblint_path))
    llvm_path = os.path.abspath(os.path.expanduser(llvm_path))
    input_path = os.path.abspath(os.path.expanduser(input_path)) #TODO Handle git url

    # Generate the programs
    goblint_executable_path = os.path.join(goblint_path, 'goblint')
    clang_tidy_path = os.path.join(llvm_path, 'build', 'bin', 'clang-tidy')
    temp_path = os.path.abspath(os.path.join(os.path.curdir, 'temp'))
    gernerate_programs(input_path, temp_path, clang_tidy_path, goblint_executable_path, 'TODO API KEY PATH', input_path, mutations, is_mutation, is_ml, is_git)

    #Write out custom test files
    print(SEPERATOR)
    print('Writing out custom test files:')
    generate_tests(temp_path, os.path.join(os.path.curdir, '99-test'), precision_test=False) #TODO Custom name
    if create_precision:
        print(SEPERATOR)
        print('Writing out custom precision files:')
        generate_tests(temp_path, os.path.join(os.path.curdir, '98-precision'), precision_test=False) #TODO Custom name

    if is_run_tests:
        test_path = os.path.abspath(os.path.join(os.path.curdir, '99-temp'))
        if create_precision:
            print(SEPERATOR)
            print('Writing out precision test files for running:')
            generate_tests(temp_path, test_path, precision_test=True)
            run_tests(test_path, goblint_path, cfg=True) #TODO Add Option for cfg
        print(SEPERATOR)
        print('Writing out test files for running:')
        generate_tests(temp_path, test_path, precision_test=False)
        run_tests(test_path, goblint_path, cfg=True) #TODO Add Option for cfg
        if os.path.exists(test_path):
            shutil.rmtree(test_path)

    #TODO Print link to html result and give summary

def cli(enable_mutations, enable_ml, enable_git, mutations, precision, running, input):
    # Check config file
    config_path = Path("config.yaml")
    config = {}
    if not config_path.is_file():
        print(f'Config file "{config_path}" not found. Please provide the paths:')
        goblint_path = questionary.text('Enter the path to the goblint repository: ', default="~/Goblint-Repo/analyzer").ask()
        llvm_path = questionary.text('Enter the path to the llvm repository with the modified clang-tidy: ', default="~/Clang-Repo/llvm-project").ask()
        config.update({"goblint-path": goblint_path, "llvm-path": llvm_path})
        with open(config_path, 'w') as outfile:
            yaml.dump(config, outfile)
    else:
        with open(config_path, 'r') as stream:
            config = yaml.safe_load(stream)
            goblint_path = config["goblint-path"]
            llvm_path = config["llvm-path"]
            print(f'Using goblint-path (change in ./config.yaml): {goblint_path}')
            print(f'Using llvm-path (change in ./config.yaml): {llvm_path}')

    # Handle Questions
    if not (enable_mutations or enable_ml or enable_git):
        while True:
            generators = questionary.checkbox(
                'Select one or more generator types (When git is checked no other can be checked!):',
                choices=[
                    questionary.Choice('Mutations', checked=True),
                    questionary.Choice('ML', checked=True),
                    'Git'
                ]).ask()

            # check if 'Git' is selected along with other options
            if 'Git' in generators and len(generators) > 1:
                print("If 'Git' is selected, no other options should be selected. Please select again.")
                continue
            else:
                break
        if 'Mutations' in generators:
            selected_mutations = questionary.checkbox(
            'Select one or more mutation types:',
            choices=[
                questionary.Choice('remove-function-body (RFB)', checked=True),
                questionary.Choice('unary-operator-inversion (UOI)', checked=True),
                questionary.Choice('relational-operator-replacement (ROR)', checked=True),
                questionary.Choice('constant-replacement (CR)', checked=True),
                questionary.Choice('remove-thread (RT)', checked=True),
                questionary.Choice('logical-connector-replacement (LCR)', checked=True),
            ]).ask()
            mutations = Mutations(
                rfb='remove-function-body (RFB)' in selected_mutations,
                uoi='unary-operator-inversion (UOI)' in selected_mutations,
                ror='relational-operator-replacement (ROR)' in selected_mutations,
                cr='constant-replacement (CR)' in selected_mutations,
                rt='remove-thread (RT)' in selected_mutations,
                lcr='logical-connector-replacement (LCR)' in selected_mutations
            )
        enable_mutations = 'Mutations' in generators
        enable_ml = 'ML' in generators
        enable_git = 'Git' in generators
    if precision == None:
        precision = questionary.confirm('Create precision test files?', default=False).ask()
    if running == None:
        running = questionary.confirm('Run the tests?').ask()
    if input == None:
        if enable_mutations or enable_ml:
            input = questionary.text('Enter the path to the c program for the mutations: ', default="input.c").ask()
        else:
            input = questionary.text('Enter the path or URL to the git repository for the mutations: ', default="repo").ask()

    run(goblint_path, llvm_path, input, enable_mutations, enable_ml, enable_git, mutations, precision, running)


if __name__ == "__main__":
    print(logo)

    parser = argparse.ArgumentParser(description='Generates mutations for creating incremental tests')
    parser.add_argument('-m', '--enable-mutations', action='store_true', help='Enable Mutations. When no mutation is selected all are activated.')
    parser.add_argument('-o', '--enable-ml', action='store_true', help='Enable ML')
    parser.add_argument('-g', '--enable-git', action='store_true', help='Enable Git')
    parser.add_argument('-ep', '--enable-precision', action='store_true', help='Enable Precision Tests')
    parser.add_argument('-dp', '--disable-precision', action='store_true', help='Disable Precision Tests')
    parser.add_argument('-er', '--enable-running', action='store_true', help='Enable Running Tests')
    parser.add_argument('-dr', '--disable-running', action='store_true', help='Disable Running Tests')
    parser.add_argument('-i', '--input', help='Input File')
    
    # Add mutation options
    add_mutation_options(parser)

    args = parser.parse_args()

    if args.enable_mutations or args.enable_ml or args.enable_git:
        # If using git, only git can be used
        if args.enable_git and (args.enable_ml or args.enable_mutations):
            parser.error("--enable-git cannot be used with --enable-ml or --enable-mutations")

        # If all mutation options are false, set all to true
        mutations = get_mutations_from_args(args)
        non_str_attributes = [attr for attr in vars(mutations) if not attr.endswith('_s')]
        if all(getattr(mutations, attr) is False for attr in non_str_attributes):
            mutations = Mutations(True, True, True, True, True, True)
    else:
        args.enable_mutations = None
        args.enable_ml = None
        args.enable_git = None
        mutations = None

    if args.enable_precision or args.disable_precision:
        # Only one can be selected
        if args.enable_precision and args.disable_precision:
            parser.error('Precision can not be enabled AND diabled')
        precision = args.enable_precision
    else:
        precision = None

    if args.enable_running or args.disable_running:
        # Only one can be selected
        if args.enable_running and args.disable_running:
            parser.error('Running can not be enabled AND diabled')
        running = args.enable_running
    else:
        running = None

    cli(args.enable_mutations, args.enable_ml, args.enable_git, mutations, precision, running, args.input)