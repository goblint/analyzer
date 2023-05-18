import os

# Change these variables to match your setup
CLANG_PATH = "~/BA/Clang/llvm-project/build/bin"
CHECKS_PATH = "~/BA/removeBody.so"

# The name of the file to check
file_name = "test.c"

# Build the clang command to run the removeBody check
clang_command = [
    "clang",
    "-Xclang",
    "-load",
    "-Xclang",
    CHECKS_PATH,
    "-Xclang",
    "-add-plugin",
    "-Xclang",
    "removeBody",
    file_name,
]

# Run the command
os.system(" ".join(clang_command))