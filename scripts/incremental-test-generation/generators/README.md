# Mutation Generators
This folder contains Python scripts that generate mutated programs. The different files and their purpose is shortly described in the following:
You can use for all python scripts the option `-h` to get additional information about the comand line arguments.
<br><br>
You can use this content for the `meta.yaml` file you need to pass:
```
n: 0
```

# generate_mutations.py
This script uses the custom clang-tidy checks to generate mutations on a given program. Informations about the mutations are written to the `meta.yaml` file.
<br><br>
More information about the mutations can be found in this file: [Mutations](../../clang-mutations/MUTATIONS.md)

# generate_ml.py
This script uses gpt-3.5-turbo with the api from openai. It generates mutations on a program by asking it how a previous version of the code could have looked like before some typical code changes were done by developers. Informations about the mutations are written to the `meta.yaml` file.
<br><br>
You need to pass a `apikey.yaml` file with the following format:
```
organisation: (Found at https://platform.openai.com/account/org-settings)
api-key: (Found at https://platform.openai.com/account/api-keys)
```
You can specify `num_selected_lines` to tell the script how many consecutive lines should be send together with the prompt. To many lines could lead to an error because of a too large request. To few lines may result in a bad mutation because of less context.
<br><br>
You can specify `interesting_lines` to guide the selection of the lines send with the request. The selection process works by selecting a random start line out of a set of lines. From the start line on the `num_selected_lines` are selected. When `interesting_lines` equals to `[]` all the lines (`[1, 2, 3, ..., MAX_LINE - NUM_SELECTED_LINES]`) are intresting lines. Alternatively you can pass specific lines (`[1, 42, 99]`). Note that when a line is larger then `(MAX_LINE - NUM_SELECTED_LINES)` it will be set to this value.

# generate_git.py
This script generates from a git repository per commit a cil file representing the whole project in a single c file. The differences between the commits are used as mutations. For passing a repository you need to specify a shell script containing all the needed information and optionally you can give commands that should be executed before and after the default build commands (`cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON` / `bear -- make`).
<br><br>
You can find the template for the shell script with the informations in the file `generate_git_build_USER_INFO_TEMPLATE.sh`:
```
#!/bin/bash

##############################################################
####################### USER VARIABLES #######################
##############################################################
# Variables
git_url=""          #e.g.: https://github.com/madler/zlib.git
use_cmake=true             # Choose either cmake or make
use_make=false             # Choose either cmake or make
path_to_build=""           #e.g.: "." (Relative path in repo)

# Functions before and after build
pre_build_commands() {
  :                        #e.g.: ./configure
}

post_build_commands() {
  :
}
##############################################################
####################### USER VARIABLES #######################
##############################################################

# Export variables so they can be used in the main script
export git_url
export use_cmake
export use_make
export path_to_build
export pre_build_commands
export post_build_commands
```

The script `generate_git_build.sh` interacts with these user shell scripts and can clone the repositories, build the repositories and provide the build path.
<br><br>
For not analyzing all the commits you can specify a start and end commit hash.