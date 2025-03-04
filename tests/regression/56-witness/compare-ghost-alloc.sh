#!/bin/bash

# Check if the correct number of arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <file1> <file2>"
    exit 1
fi

file1="$1"
file2="$2"

# Function to extract the first and second occurrences of the pattern
extract_variables() {
    grep -o -m 2 'alloc_m[0-9]\+_locked' "$1"
}

# Extract variables from both files
var1_file1=$(extract_variables "$file1" | sed -n '1p')
var2_file1=$(extract_variables "$file1" | sed -n '2p')
var1_file2=$(extract_variables "$file2" | sed -n '1p')
var2_file2=$(extract_variables "$file2" | sed -n '2p')

# Check if the variables were found
if [ -z "$var1_file1" ] || [ -z "$var2_file1" ] || [ -z "$var1_file2" ] || [ -z "$var2_file2" ]; then
    echo "Error: Could not find the required pattern in one or both files."
    exit 1
fi

# Create temporary files
temp1=$(mktemp)
temp2=$(mktemp)

# Rename variables in both files
sed -e "s/\b$var1_file1\b/TEMP_VAR1/g" -e "s/\b$var2_file1\b/TEMP_VAR2/g" "$file1" > "$temp1"
sed -e "s/\b$var1_file2\b/TEMP_VAR1/g" -e "s/\b$var2_file2\b/TEMP_VAR2/g" "$file2" > "$temp2"

# Compare the modified files
if diff -q "$temp1" "$temp2" > /dev/null; then
    echo "The files are the same after renaming the variables."
else
    echo "The files are different after renaming the variables."
fi

# Clean up temporary files
rm "$temp1" "$temp2"
