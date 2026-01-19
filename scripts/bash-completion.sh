#!/usr/bin/env bash

# Temporary usage:
#   Run: source ./scripts/bash-completion.sh
#
# Permanent usage:
#   Run: echo "source $(readlink -f .)/scripts/bash-completion.sh" >> ~/.bash_completion

# Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
# Copied & modified from standard __ltrim_colon_completions
__ltrim_equal_completions()
{
    if [[ $1 == *=* && $COMP_WORDBREAKS == *=* ]]; then
        # Remove equal-word prefix from COMPREPLY items
        local equal_word=${1%"${1##*=}"}
        local i=${#COMPREPLY[*]}
        while ((i-- > 0)); do
            COMPREPLY[i]=${COMPREPLY[i]#"$equal_word"}
        done
    fi
}

_goblint ()
{
    IFS=$'\n'
    local words cword cur
    _get_comp_words_by_ref -n = cur words cword # Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
    COMPREPLY=($(${words[0]/#\~/$HOME} --complete "${words[@]:1:cword}"))
    __ltrim_equal_completions "$cur" # Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
}

complete -o default -F _goblint goblint


_regtest ()
{
    IFS=$'\n'
    case $COMP_CWORD in
        1)
            COMPREPLY=($(ls -1 tests/regression/ | sed -n -r 's/([0-9][0-9])-.*/\1/p' | grep "^${COMP_WORDS[1]}"))
            ;;
        2)
            COMPREPLY=($(ls -1 tests/regression/${COMP_WORDS[1]}-* | sed -n -r 's/([0-9][0-9])-.*/\1/p' | grep "^${COMP_WORDS[2]}"))
            ;;
        *)
            local words cword cur
            _get_comp_words_by_ref -n = cur words cword # Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
            COMPREPLY=($($(dirname ${words[0]/#\~/$HOME})/goblint --complete "${words[@]:3:cword}"))
            __ltrim_equal_completions "$cur" # Bypass = in COMP_WORDBREAKS (https://stackoverflow.com/a/57437406/854540)
            ;;
    esac
}

complete -o default -F _regtest regtest.sh


_update_suite ()
{
    IFS=$'\n'
    case $COMP_CWORD in
        1)
            COMPREPLY=($(ls -1 tests/regression/*/*.c | sed -n -r 's|.*/([0-9][0-9])-(.*)\.c|\2|p' | grep "^${COMP_WORDS[1]}"))
            if [[ "group" =~ ^${COMP_WORDS[1]} ]]; then
                COMPREPLY+=("group")
            fi
            ;;
        2)
            if [[ ${COMP_WORDS[1]} == "group" ]] ; then
                COMPREPLY=($(ls -1 tests/regression/ | sed -n -r 's/([0-9][0-9])-(.*)/\2/p' | grep "^${COMP_WORDS[2]}"))
            else
                COMPREPLY=()
            fi
            ;;
        *)
            COMPREPLY=()
            ;;
    esac
}

complete -F _update_suite update_suite.rb
