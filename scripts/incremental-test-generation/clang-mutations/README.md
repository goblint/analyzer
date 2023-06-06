# Create Clang-Tidy Checks for mutations
In this document is described how you can create all the Clang-Tidy checks needed for generating the code mutations.

## Dependencies
For building Clang you need to install some dependencies:

 - [ ] `sudo apt install ninja-build`
 - [ ] `sudo apt install ccache`
 - [ ] `sudo apt install lld`

## Cloning the repository
 - [ ] There are two alternatives for getting the repository

For creating all the checks by yourself clone the **Official Clang Repository**:
`git clone https://github.com/llvm/llvm-project.git` (Tested with Version 17.0.0)
Alternatively you can clone the **Fork** with all the additional checks ready:
`git clone https://github.com/J2000A/llvm-project.git`

## Creating the checks
 - [ ] When you cloned the Official Clang Repository you need to add the checks. Otherwise you can skip this part.
 - [ ] Move to the directory `llvm-project/clang-tools-extra`

In this directory are the implementations of the checks with their corresponding `.cpp` and `.h` file. In the following we will use the **>>check-name<<** of each check. You can get it by the filename without the word "Check" at the end and changing the capital letters to small ones with a minus in front (e.g. the >>check-name<< of `RemoveFunctionBodyCheck.cpp` is `remove-function-body`). Repeat the following steps for all new checks.

 - [ ] Run `clang-tidy/add_new_check.py readability >>check-name<<`
 - [ ] Replace `./clang-tidy/readability/>>check-name<<.cpp` with the implementation in this directory
 - [ ] Replace `./clang-tidy/readability/>>check-name<<.h` with the header file in this directory

Now you have added all the check we need for the mutations.

## Build
The first build can take a while (up to multiple hours). But you can increase the performance by changing the parallel compile and link jobs. For me using the value 5 for both of them got me the fastest results. When using too many jobs the memory becomes a bottleneck. You can check the memory status with `free -h --giga`.
Additionally you may need to change the build target. Avaiable targets are: AMDGPU, ARM, AVR, BPF, Hexagon, Lanai, LoongArch, Mips, MSP430, NVPTX, PowerPC, RISCV, Sparc, SystemZ, VE, WebAssembly, X86, XCore

 - [ ] Move to the directory `llvm-project/`
 - [ ] `mkdir build && cd build`
 - [ ] `cmake -G "Ninja" -DLLVM_CCACHE_BUILD=ON -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_LINKER=lld -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DLLVM_TARGETS_TO_BUILD="X86" -DLLVM_PARALLEL_COMPILE_JOBS=5 -DLLVM_PARALLEL_LINK_JOBS=5 ../llvm`
 - [ ] `sudo ninja install`

## Running Clang-Tidy
We will use the **>>check-name<<** again as defined in "Creating the checks".

**Example:** Create the mutation "remove function body" on a file "test.c" in lines "4" and "14" when the function name is "foo":
`clang-tidy -checks=-*,readability-remove-function-body -fix --fix-errors -config="{CheckOptions: {readability-remove-function-body.RemoveOnlyFunctionName: 'foo'}}" -line filter='[{"name":"test.c","lines":[[4,4],[14,14]]}]' test.c --`

**The command consists of the following components:**
 - [ ] Clang-Tidy
`clang-tidy` The command itself.

 - [ ] General Options
 `-checks=-*,readability->>check-name<<` Deactivating all checks except >>check-name<<.
`-fix` Applying the mutations.
`--fix-errors` Apply also when errors where detected
`-line filter='[{"name":"test.c","lines":[[4,4],[14,14]]}]'` Apply the mutations only on line 4 and 14.

 - [ ] Special Options
`-config="{CheckOptions: {readability-remove-function-body.RemoveOnlyFunctionName: 'foo1, foo2'}}"` Special option for the **remove-function-body** check to only remove the function body of functions named foo1 and foo2.

 - [ ] Filename
`test.c --` The filename.

## Mutations
You find more details about the different Mutations in the [Mutations](MUTATIONS.md) file.

## Workflow
First run the check without `-fix --fix-errors` to see where mutations are possible without applying them. Remember the lines where you actually want to apply the mutation. Make a copy of the input file that you will mutate. The run the check again with `-fix --fix-errors` and `-line filter=...` on the copied file to apply only specific mutations and not all at ones.