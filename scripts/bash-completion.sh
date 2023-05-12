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

complete -o default -F _goblint goblint


_regtest ()
{
    IFS=$'\n'
    case $COMP_CWORD in
        1)
            COMPREPLY=($(ls -1 tests/regression/ | sed -n -r 's/([0-9][0-9])-.*/\1/p'))
            ;;
        2)
            COMPREPLY=($(ls -1 tests/regression/${COMP_WORDS[1]}-* | sed -n -r 's/([0-9][0-9])-.*/\1/p'))
            ;;
        *)
            COMPREPLY=($($(dirname ${COMP_WORDS[0]})/goblint --complete "${COMP_WORDS[@]:3:COMP_CWORD}"))
            ;;
    esac
}

complete -o default -F _regtest regtest.sh
