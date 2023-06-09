#!/bin/bash

##############################################################
####################### USER VARIABLES #######################
##############################################################
# Variables
git_url="https://github.com/madler/zlib.git"
use_cmake=false
use_make=true
path_to_build="."

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

# Export variables so they can be used in the main script
export git_url
export use_cmake
export use_make
export path_to_build
export pre_build_commands
export post_build_commands