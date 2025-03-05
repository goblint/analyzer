#!/bin/bash

# Check if the correct number of arguments are provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

file="$1"

# Function to extract the first and second occurrences of the pattern
extract_variables() {
    grep -o -m 2 'alloc_m[0-9]\+_locked' "$1"
}

# Extract variables from the file
var1=$(extract_variables "$file" | sed -n '1p')
var2=$(extract_variables "$file" | sed -n '2p')

# Check if both variables were found
if [ -z "$var1" ] || [ -z "$var2" ]; then
    echo "Error: Could not find two occurrences of the pattern 'alloc_m[0-9]+_locked' in the file."
    exit 1
fi

# Rename variables and print the modified file to stdout
sed -e "s/\b$var1\b/ALLOC_VAR1_LOCKED/g" -e "s/\b$var2\b/ALLOC_VAR2_LOCKED/g" "$file"
