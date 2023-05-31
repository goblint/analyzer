# Running Goblint

## Basic Execution

To analyze a C-source file with Goblint, you may simply pass the path to the file as a command line argument to it.
To generate an HTML-output that can be [inspected using the browser](inspecting.md), additionally pass the `--html` flag.

#### Example:

```console
./goblint tests/regression/01-cpa/02-branch.c --html
```
## Passing Options

Goblint offers many options to tweak how the analysis is performed.
For more information on how to set options, run:

```console
./goblint --help
```

For a list of all options and their possible configurations, run:

```console
./goblint --print_all_options
```

To use one of the pre-defined configurations, run:

```console
./goblint --conf path/to/config.json files
```

## Analyzing Recursive Programs
In some cases, when using the default configuration, Goblint might not terminate in reasonable time on recursive programs, or
crash in a stack overflow (indicated by the error message `exception Stack overflow`). If the stack overflow occurs within a C function called by Goblint, it will result in the following error message: `Command terminated by signal 11`.

Adding the option `--enable ana.context.widen` will enable widening on the contexts in which functions are analyzed. This avoids stack overflows possibly caused by the analysis of recursive functions.


## Project analysis

To analyze real-world projects, [Compilation Databases](https://clang.llvm.org/docs/JSONCompilationDatabase.html) can be used.
First, generate a compilation database `compile_commands.json`:

* For CMake projects, add `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` argument to `cmake`.
* For Make projects:
    * Use [Bear](https://github.com/rizsotto/Bear): `bear -- make` (preferred).
    * Or use [compiledb](https://github.com/nickdiego/compiledb): `compiledb make`.

Second, run Goblint on the compilation database:
```console
goblint compile_commands.json
```

### Caveats
Here is a list of issues and workarounds for different compilation database generators we have encounted.

#### compiledb
1. Doesn't properly escape `-D` argument values (<https://github.com/goblint/bench/issues/8#issuecomment-1017538298>, <https://github.com/goblint/bench/issues/14#issue-1112574635>).
2. Errors with `.deps` and Makefile targets (<https://github.com/goblint/bench/issues/17>).
    * Workaround is to remove the manually.
3. Assumes English locale (GobCon 23.02.2022 notes).
4. Doesn't completely decompose multi-file targets (<https://github.com/goblint/analyzer/issues/624#issuecomment-1060566770>).
5. Uses relative paths for preprocessing and thus in AST.

#### bear
1. Bear 2.3.11 from Ubuntu 18.04 produces incomplete database (<https://github.com/goblint/bench/issues/7#issuecomment-1011055984>, <https://github.com/goblint/bench/issues/7#issuecomment-1011987188>).
    * Bear 3.0.8 seems fine.
