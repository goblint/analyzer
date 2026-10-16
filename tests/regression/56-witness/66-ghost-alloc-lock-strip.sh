#!/bin/bash

# Check if the correct number of arguments are provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

file="$1"

# Extract variables from the file
var1=$(sed -nr "s/.*(alloc_m[0-9]+_locked).*g1.*/\1/p" "$file")
var2=$(sed -nr "s/.*(alloc_m[0-9]+_locked).*g2.*/\1/p" "$file")

# Check if both variables were found
if [ -z "$var1" ] || [ -z "$var2" ]; then
    echo "Error: Could not find two occurrences of the pattern 'alloc_m[0-9]+_locked' in the file."
    exit 1
fi

# Rename variables and print the modified file to stdout
sed -e "s/$var1/ALLOC_VAR1_LOCKED/g" -e "s/$var2/ALLOC_VAR2_LOCKED/g" "$file"
