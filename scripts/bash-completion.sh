#!/usr/bin/env bash

# Temporary usage:
#   Run: source ./scripts/bash-completion.sh
#
# Permanent usage:
#   Run: echo "source $(readlink -f .)/scripts/bash-completion.sh" >> ~/.bash_completion

_goblint ()
{
    IFS=$'\n'
    COMPREPLY=($(${COMP_WORDS[0]} --complete "${COMP_WORDS[@]:1:COMP_CWORD}"))
}

complete -o default -F _goblint goblint ./goblint
