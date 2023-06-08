#!/bin/bash

# Get the user infor about building the git file
if [[ -n $1 ]]; then
  # Source the input script from the provided path
  source $1
else
  echo "Please give the path to a build info sh file as an argument."
  echo "The file should look like this:"
  echo ""
  cat generate_git_build_USER_INFO_PRESET.sh
  echo ""
  exit 1
fi

# Exit if any command fails
set -e

# Export path for Mutation Generator
if [ "$2" == "--path" ]; then
  echo "$(pwd)/$path_to_execute_cil"
  exit 0
fi

# Check bool variables
if { $use_cmake && $use_make ; } || { ! $use_cmake && ! $use_make ; }; then
  echo "Error: Either cmake or make should be used, but not both or neither."
  exit 1
fi

# Main script
repo_name=$(basename "$git_url" .git)
rm -rf $repo_name
git clone $git_url

cd "$(dirname "$0")"
cd $path_to_build
pre_build_commands

if $use_cmake; then
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
elif $use_make; then
  bear -- make
fi

post_build_commands
