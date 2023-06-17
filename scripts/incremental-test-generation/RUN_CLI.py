import argparse
import os
import sys
import questionary
import yaml
from pathlib import Path
from util.util import *
from util.generate_programs import generate_programs
from util.generate_tests import generate_tests
from util.run_tests import run_tests
from generators.generate_mutations import add_mutation_options, get_mutations_from_args
from generators.generate_ml import validate_interesting_lines

logo = '''
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

def run(goblint_path, llvm_path, input_path, is_mutation, is_ml, is_git, mutations, goblint_config, test_name, create_tests, enable_precision, precision_name, is_run_tests, api_key_path, ml_count, ml_select, ml_interesting, ml_16k, cfg, git_start, git_end):
    # Make paths absolute
    goblint_path = os.path.abspath(os.path.expanduser(goblint_path))
    llvm_path = os.path.abspath(os.path.expanduser(llvm_path))
    input_path = os.path.abspath(os.path.expanduser(input_path))

    # Generate the programs
    goblint_executable_path = os.path.join(goblint_path, 'goblint')
    clang_tidy_path = os.path.join(llvm_path, 'build', 'bin', 'clang-tidy')
    temp_path = os.path.abspath(os.path.join(os.path.curdir, 'temp'))
    generate_programs(input_path, temp_path, clang_tidy_path, goblint_executable_path, api_key_path, mutations, is_mutation, is_ml, is_git, ml_count, ml_select, ml_interesting, ml_16k, git_start, git_end)

    # Run tests
    if is_run_tests:
        test_path = os.path.abspath(os.path.join(temp_path, '100-temp'))
        if enable_precision:
            print(SEPERATOR)
            print(f'Running {COLOR_BLUE}PRECISION TEST{COLOR_RESET}:')
            paths = generate_tests(temp_path, test_path, goblint_config, precision_test=True, temp_name=True)
            if len(paths) > 1:
                print(f"{COLOR_YELLOW}[INFO] There were more than 99 programs generated, so the tests had to be spitted into multiple directories{COLOR_RESET}")
            for path in paths:
                run_tests(input_path, path, goblint_path, cfg)
        print(SEPERATOR)
        print(f'Running {COLOR_BLUE}CORRECTNESS TEST{COLOR_RESET}:')
        paths = generate_tests(temp_path, test_path, goblint_config, precision_test=False, temp_name=True)
        if len(paths) > 1:
                print(f"{COLOR_YELLOW}[INFO] There were more than 99 programs generated, so the tests had to be spitted into multiple directories{COLOR_RESET}")
        for path in paths:
            run_tests(input_path, path, goblint_path, cfg)

    # Write out custom test files
    if create_tests:
        print(SEPERATOR)
        correctness_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'out', test_name)
        print(f'Writing out {COLOR_BLUE}CUSTOM CORRECTNESS TEST {test_name}{COLOR_RESET} files:')
        paths = generate_tests(temp_path, correctness_path, goblint_config, precision_test=False, temp_name=False)
        if len(paths) > 1:
                print(f"{COLOR_YELLOW}[INFO] There were more than 99 programs generated, so the tests had to be spitted into multiple directories{COLOR_RESET}")
        for path in paths:
            print(f'{COLOR_GREEN}Test stored in the file: {path}{COLOR_RESET}')
        if enable_precision:
            print(SEPERATOR)
            precision_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'out', precision_name)
            print(f'Writing out {COLOR_BLUE}CUSTOM PRECISION TEST {precision_name}{COLOR_RESET} files:')
            paths = generate_tests(temp_path, precision_path, goblint_config, precision_test=False, temp_name=False)
            if len(paths) > 1:
                print(f"{COLOR_YELLOW}[INFO] There were more than 99 programs generated, so the tests had to be spitted into multiple directories{COLOR_RESET}")
            for path in paths:
                print(f'{COLOR_GREEN}Test stored in the file: {path}{COLOR_RESET}')

def cli(enable_mutations, enable_ml, enable_git, mutations, goblint_config, test_name, create_tests, enable_precision, precision_name, running, input, ml_count, ml_select, ml_interesting, ml_16k, cfg, git_start, git_end, git_no_commit):
    # Check config file
    config_path = Path(CONFIG_FILENAME)
    config = {}
    if not config_path.is_file():
        print(f'Config file "{config_path}" not found. Please provide the paths:')
        goblint_path = questionary.text('Enter the path to the goblint repository: ', default="~/Goblint-Repo/analyzer").ask()
        llvm_path = questionary.text('Enter the path to the llvm repository with the modified clang-tidy: ', default="~/Clang-Repo/llvm-project").ask()
        config.update({CONFIG_GOBLINT: goblint_path, CONFIG_LLVM: llvm_path, CONFIG_LAST_INPUT_MUTATION: '', CONFIG_LAST_INPUT_GIT: ''})
        last_input_mutation = ''
        last_input_git = ''
        with open(config_path, 'w') as outfile:
            yaml.dump(config, outfile)
    else:
        with open(config_path, 'r') as stream:
            config = yaml.safe_load(stream)
            goblint_path = config[CONFIG_GOBLINT]
            llvm_path = config[CONFIG_LLVM]
            last_input_mutation = config[CONFIG_LAST_INPUT_MUTATION]
            last_input_git = config[CONFIG_LAST_INPUT_GIT]
            print(f'Using goblint-path (change in ./{CONFIG_FILENAME}): {goblint_path}')
            print(f'Using llvm-path (change in ./{CONFIG_FILENAME}): {llvm_path}')

    # Handle Questions
    if not (enable_mutations or enable_ml or enable_git):
        while True:
            generators = questionary.checkbox(
                'Select one or more generator types (When git is checked no other can be checked!):',
                choices=[
                    questionary.Choice('Mutations', checked=True),
                    'ML',
                    'Git'
                ]).ask()

            # check if 'Git' is selected along with other options
            if 'Git' in generators and len(generators) > 1:
                print(f"{COLOR_RED}If 'Git' is selected, no other options should be selected. Please select again.{COLOR_RESET}")
                continue
            else:
                break

        enable_mutations = 'Mutations' in generators
        enable_ml = 'ML' in generators
        enable_git = 'Git' in generators
        
        if enable_mutations:
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
    
    # Check for API Key
    if enable_ml:
        key_path = Path(APIKEY_FILENAME)
        key_data = {}
        if not key_path.is_file():
            print(f'Api key file "{key_path}" for OpenAi not found. Please provide the informations:')
            print('Be aware that the information is stored unencrypted. Do not remove the file from .gitignore!')
            print('Create an account here: https://openai.com/blog/openai-api')
            print('Create an API Key here: https://platform.openai.com/account/api-keys')
            print('Get your organization id here: https://platform.openai.com/account/org-settings')
            key = questionary.text('Enter the api key:').ask()
            org = questionary.text('Enter the organisation id:').ask()
            key_data.update({APIKEY_APIKEY: key, APIKEY_ORGANISATION: org})
            with open(key_path, 'w') as outfile:
                yaml.dump(key_data, outfile)
        else:
            with open(key_path, 'r') as stream:
                key_data = yaml.safe_load(stream)
                key = key_data[APIKEY_APIKEY]
                org = key_data[APIKEY_ORGANISATION]
                print(f'Using api-key for ML (change in ./{APIKEY_FILENAME}): ...{key[-4:]}')
                print(f'Using organisation id for ML (change in ./{APIKEY_FILENAME}): ...{org[-4:]}')
        key_path = os.path.abspath(key_path)
    else:
        key_path = None

    # Check for config file
    if goblint_config == None:
        goblint_config = questionary.text('Path to a goblint config file used to create tests. Passing {} creates an empty config file.', default='{}').ask()
    if goblint_config == '{}' or goblint_config == '':
        goblint_config = None

    # ML Options
    if enable_ml and ml_count == None:
        while True:
            ml_count = questionary.text('How many different programs should be generated with ML?', default=str(DEFAULT_ML_COUNT)).ask()
            if not ml_count.strip('\n').isdigit():
                print(f"{COLOR_RED}Please enter a valid number.{COLOR_RESET}")
                continue
            ml_count = int(ml_count.strip('\n'))
            if ml_count <= 0:
                print(f"{COLOR_RED}Please enter a number greater zero.{COLOR_RESET}")
                continue
            break

    if enable_ml and ml_select == None:
        while True:
            ml_select = questionary.text('How many lines should be selected for the snippet from the input file?', default=str(DEFAULT_ML_SELECT)).ask()
            if not ml_select.strip('\n').isdigit():
                print(f"{COLOR_RED}Please enter a valid number.{COLOR_RESET}")
                continue
            ml_select = int(ml_select.strip('\n'))
            if ml_select <= 0:
                print(f"{COLOR_RED}Please enter a number greater zero.{COLOR_RESET}")
                continue
            break

    if enable_ml and ml_16k == None:
        ml_16k = questionary.confirm('Use the gpt-3.5-turbo-16k model instead of the gpt-3.5-turbo model', default=False).ask()

    if enable_ml and ml_interesting == None:
        while True:
            ml_interesting = questionary.text('From which start lines should the snippet start be choosen randomly ([] stands for all)?', default='[]').ask()
            if validate_interesting_lines(ml_interesting, None) == None:
                print(f'{COLOR_RED}Please enter a valid string like [1, 42], [99], ....{COLOR_RESET}')
                continue
            break

    # Git options
    if enable_git and not (git_start != None and git_end != None) and not git_no_commit:
        if questionary.confirm('Do you want to give a start and end commit hash?', default=False).ask():
            git_start = questionary.text('Enter start commit hash:').ask()
            git_end = questionary.text('Enter end commit hash:').ask()

    # Output options
    if create_tests == None:
        create_tests = questionary.confirm('Create test files?', default=False).ask()

    if create_tests and test_name == None:
        while True:
            test_name = questionary.text('Enter the test name: ', default="90-test").ask()
            if check_test_name(test_name):
                break

    if enable_precision == None:
        enable_precision = questionary.confirm('Enable precision tests?', default=False).ask()

    if create_tests and enable_precision and precision_name == None:
        while True:
            precision_name = questionary.text('Enter the precision test name: ', default="80-precision").ask()
            if check_test_name(precision_name):
                break

    if running == None:
        running = questionary.confirm('Run the tests?').ask()

    if running and cfg == None:
        cfg = questionary.confirm('Run the fine grained cfg tests?').ask()

    # input options
    if input == None:
        while True:
            if enable_mutations or enable_ml:
                input = questionary.text('Enter the path to the c program for the mutations: ', default=last_input_mutation).ask()
                config.update({CONFIG_LAST_INPUT_MUTATION: input})
            else:
                input = questionary.text('Enter the path to the sh script with informations about the git repository (Use [-s] to see the template script ): ', default=last_input_git).ask()
                config.update({CONFIG_LAST_INPUT_GIT: input})
            if not os.path.exists(input):
                print(f"{COLOR_RED}Please enter a valid path.{COLOR_RESET}")
                continue
            with open(config_path, 'w') as outfile:
                yaml.dump(config, outfile)
            break

    run(goblint_path, llvm_path, input, enable_mutations, enable_ml, enable_git, mutations, goblint_config, test_name, create_tests, enable_precision, precision_name, running, key_path, ml_count, ml_select, ml_interesting, ml_16k, cfg, git_start, git_end)


if __name__ == "__main__":
    print(f'{COLOR_YELLOW}Use [-h] to see the command line options{COLOR_RESET}')
    print(logo)

    parser = argparse.ArgumentParser(description='Generates mutations for creating incremental tests')
    parser.add_argument('-m', '--enable-mutations', action='store_true', help='Enable Mutations. When no mutation is selected all are activated.')
    parser.add_argument('-o', '--enable-ml', action='store_true', help='Enable ML')
    parser.add_argument('-g', '--enable-git', action='store_true', help='Enable Git')
    parser.add_argument('-c', '--goblint-config', help='Path to a goblint config file used to create tests (passing "{}" as argument creates an empty config file)')
    parser.add_argument('-ep', '--enable-precision', action='store_true', help='Enable Precision Tests')
    parser.add_argument('-dp', '--disable-precision', action='store_true', help='Disable Precision Tests')
    parser.add_argument('-er', '--enable-running', action='store_true', help='Enable running tests')
    parser.add_argument('-dr', '--disable-running', action='store_true', help='Disable running tests')
    parser.add_argument('-et', '--enable-create-tests', action='store_true', help='Enable creating test files')
    parser.add_argument('-dt', '--disable-create-tests', action='store_true', help='Disable creating test files')
    parser.add_argument('-ec', '--enable-cfg', action='store_true', help='Enable fine grained cfg tests')
    parser.add_argument('-dc', '--disable-cfg', action='store_true', help='Disable fine grained cfg tests')
    parser.add_argument('-t', '--test-name', help='Test name')
    parser.add_argument('-p', '--precision-name', help='Precision test name')
    parser.add_argument('-i', '--input', help='Input File')
    
    # Add mutation options
    add_mutation_options(parser)

    # Add ML options
    parser.add_argument('-mc', '--ml-count', type=int, default=-1,  help='How many different programs should be generated with ML?')
    parser.add_argument('-ms', '--ml-select', type=int, default=-1,  help='How many lines should be selected for the snippet from the input file?')
    parser.add_argument('-mi', '--ml-interesting', help='From which start lines should the snippet start be choosen randomly? Exp. :[] = From all lines, [1, 42], ...')
    parser.add_argument('-m4', '--ml-4k', action='store_true', help='Use the gpt-3.5-turbo model instead of the gpt-3.5-turbo-16k model')
    parser.add_argument('-m16', '--ml-16k', action='store_true', help='Use the gpt-3.5-turbo-16k model instead of the gpt-3.5-turbo model')

    # Add GIT options
    parser.add_argument('-s', '--template-script', action='store_true', help='Print the template script for git repositories')
    parser.add_argument('-gs', '--git-start-commit', help='The hash of the first commit to consider')
    parser.add_argument('-ge', '--git-end-commit', help='The hash of the last commit to consider')
    parser.add_argument('-gn', '--git-no-commit', action='store_true', help='Suppress asking for commit hashes in CLI')

    args = parser.parse_args()

    if args.template_script:
        template_path = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'generators', 'generate_git_build_USER_INFO_TEMPLATE.sh'))
        print(f'{COLOR_YELLOW}Template can be found at: {template_path}{COLOR_RESET}')
        print('')
        with open(template_path, 'r') as file:
            content = file.read()
        print(content)
        print('')
        sys.exit(0)

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

    git_start_commit = args.git_start_commit
    git_end_commit = args.git_end_commit
    if (git_start_commit == None and git_end_commit != None) or (git_start_commit != None and git_end_commit == None):
        parser.error('[ERROR] Give a git start commit hash AND a end commit hash')

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

    if args.enable_create_tests or args.disable_create_tests:
        # Only one can be selected
        if args.enable_create_tests and args.disable_create_tests:
            parser.error('Create tests can not be enabled AND diabled')
        create_tests = args.enable_create_tests
    else:
        create_tests = None

    if args.enable_cfg or args.disable_cfg:
        # Only one can be selected
        if args.enable_cfg and args.disable_cfg:
            parser.error('Cfg can not be enabled AND diabled')
        cfg = args.enable_cfg
    else:
        cfg = None

    if args.ml_count > 0:
        ml_count = args.ml_count
    else:
        ml_count = None

    if args.ml_select > 0:
        ml_select = args.ml_select
    else:
        ml_select = None

    if args.ml_interesting != None and validate_interesting_lines(args.ml_interesting, None) == None:
        sys.exit(-1)

    if args.ml_4k or args.ml_16k:
        # Only one can be selected
        if args.ml_4k and args.ml_16k:
            parser.error('Only one ml model can be selected!')
        ml_16k = args.ml_16k
    else:
        ml_16k = None

    test_name = args.test_name
    if test_name != None and not check_test_name(test_name):
        sys.exit(-1)

    precision_name = args.precision_name
    if precision_name != None and not check_test_name(precision_name):
        sys.exit(-1)
    
    cli(args.enable_mutations, args.enable_ml, args.enable_git, mutations, args.goblint_config, test_name, create_tests, precision, precision_name, running, args.input, ml_count, ml_select, args.ml_interesting, ml_16k, cfg, git_start_commit, git_end_commit, args.git_no_commit)