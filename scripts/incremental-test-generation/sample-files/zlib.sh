#!/bin/bash

# Script with relevant informations for processing a git repo

##############################################################
####################### USER VARIABLES #######################
##############################################################
# Variables
git_url="https://github.com/madler/zlib.git"
use_cmake=true
use_make=false
path_to_build="zlib/"
path_to_execute_cil="zlib/"

# Functions before and after build
pre_build_commands() {
  ./configure
}

post_build_commands() {
  :
}
##############################################################
####################### USER VARIABLES #######################
##############################################################

# Exit if any command fails
set -e

# Export path for Mutation Generator
if [ "$1" == "--path" ]; then
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
