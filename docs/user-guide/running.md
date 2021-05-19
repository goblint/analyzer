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

## Analyzing Recursive Programs
In some cases, when using the default configuration, Goblint might not terminate in reasonable time on recursive programs, or
crash in a stack overflow (indicated by the error message `exception Stack overflow`). If the stack overflow occurs within a C function called by Goblint, it will result in the following error message: `Command terminated by signal 11`.

Adding the option `--enable exp.widen-context` will enable widening on the contexts in which functions are analyzed. This avoids stack overflows possibly caused by the analysis of recursive functions.
