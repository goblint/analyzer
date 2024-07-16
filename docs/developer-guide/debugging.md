# Debugging

## Logging
Instead of debug printing directly to `stdout`, all logging should be done using the `Logs` module.
This allows for consistent pretty terminal output, as well as avoiding interference with server mode.
There are five logging levels: result, error, warning, info and debug.
Log output is controlled by the `dbg.level` option, which defaults to "info".

Logs are written to `stderr`, except for result level, which go to `stdout` by default.

Goblint extensively uses [CIL's `Pretty`](https://people.eecs.berkeley.edu/~necula/cil/api/Pretty.html) module for output due to many non-primitive values.

* Logging CIL values (e.g. an expression `exp`) using the corresponding pretty-printer `d_exp` from `Cil` module:

```ocaml
Logs.debug "A CIL exp: %a" d_exp exp;
```

* Logging Goblint's `Printable` values (e.g. a domain `D` element `d`) using the corresponding pretty-printer `D.pretty`:

```ocaml
Logs.debug "A domain element: %a" D.pretty d;
```

* Logging primitives (e.g. OCaml ints, strings, etc) using the standard [OCaml `Printf`](https://ocaml.org/api/Printf.html) specifiers:

```ocaml
Logs.debug "An int and a string: %d %s" 42 "magic";
```

* Logging lists of pretty-printables (e.g. expressions list `exps`) using `d_list`:

```ocaml
Logs.debug "Some expressions: %a" (d_list ", " d_exp) exps;
```


## Tracing
Tracing is a nicer alternative to some logging, because it can be disabled for best performance and it can be used to only see relevant tracing output.

Recompile with tracing enabled: `./scripts/trace_on.sh`.

Instead of logging use a tracing function from the `Messages` module, which is often aliased to just `M` (and pick a relevant name instead of `mything`):
```ocaml
if M.tracing then M.trace "mything" "A domain element: %a" D.pretty d;
```

Then run Goblint with the additional argument `--trace mything`.
If the traced code runs often, it's best to pipe Goblint's output to a file.

Other tracing functions are available:

* `M.tracel` also includes the analysed program location.
* `M.tracei` and `M.traceu` can be used to indend and unindent tracing output.

## Running Goblint in a Debugger
### Building a Debuggable Goblint Executable

To build a Goblint executable with debug information, run the following command within the `analyzer` directory.

```console
make byte
```

This will create a file called `goblint.byte`.

### Debugging Goblint with VS Code

To debug OCaml programs, you can use the command line interface of `ocamldebug` or make use of the Visual Studio Code
integration provided by `ocamllabs.ocaml-platform`.
In the following, we describe the steps necessary to set up this VS Code extension to
debug Goblint.

### Setting-up Earlybird

Install the [`ocamllabs.ocaml-platform` extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform) in your installation of Visual Studio Code.
To be able to use this extension, you additionally need to install `earlybird` on the opam switch you use for Goblint.
To do so, run the following command in the `analyzer` directory:

```console
opam install earlybird
```

### Providing a Launch Configuration

To let the `ocamllabs.ocaml-platform` extension know which executable it should debug, and which arguments it should pass, we have to provide a configuration file.
The configuration file has to be named `launch.json` and must reside in the `./.vscode` directory. Here is an example `launch.json`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
      {
        "name": "Goblint",
        "type": "ocaml.earlybird",
        "request": "launch",
        "program": "${workspaceFolder}/goblint.byte",
        "arguments": [
          "tests/regression/00-sanity/01-assert.c",
          "--enable", "ana.int.interval",
        ],
        "env": {
          "LD_LIBRARY_PATH": "$LD_LIBRARY_PATH:_build/default/src/common"
        },
        "stopOnEntry": false,
      }
    ]
}
```
Note that the individual arguments to Goblint should be passed here as separate strings that do not contain spaces. Finally, to enable breakpoints uncomment `(map_workspace_root false)` in the dune-project file.


### Running Goblint in the VS Code Debugger

To make sure that VS Code can find `ocamlearlybird`, run the following commands in the `analyzer` directory:

```console
eval $(opam env) // Sets up envrionment variables
code .           // Starts VS Code in the current directory
```

After VS Code has started, you can set breakpoints in the Goblint code. You can start debugging using the VS Code command palette with the command `"Debug: Start Debugging"`, or alternatively by pressing `F5`. Note that the Goblint execution is considerably slowed down in the debugger, so you have to be somewhat patient.
Of course, don't forget to rebuild the debuggable executable if you make changes to the code!

## Debugging Issues with Larger Programs

Sometimes during development one may encounter instances where, e.g., the verifying phase reports that the fixpoint is not reached. This is usually due to bugs in `join`, `widen` or `leq`. For small programs, one can find the cause by inspecting the program and the output carefully. If the issue happens only with large programs it is hard to understand.

To work on such cases, it makes sense to reduce the program to a small example program that still triggers the same issue. This can either be done by hand or using `creduce`. `creduce` takes two inputs: a script that terminates with status `0` if the reduced program is still interesting, and the original program.

In the case of looking for issues with fixpoints not being found, such a script may e.g. be given by:

```bash
#!/bin/bash
~/path/to/goblint input.c -v &> out.txt
if [ $? -eq 3 ]; then
    grep Fixpoint out.txt >/dev/null 2>&1
else
    exit 5
fi
```

Note that Goblint exits with status `3` if the verifier fails.

Some more sophisticated scripts can be found in the folder `./scripts/creduce`.

```console
creduce --timeout 900 reduce.sh input.c
```
where timeout is set to a reasonable time in which Goblint terminates on the input program. This may run for several hours/days, so it makes sense to start it on
a server. It may also be helpful to set `--n <N>` where `N` is the number of cores to use to get a considerable speedup.
