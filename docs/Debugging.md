# Debugging Goblint

## Building a Debuggable Goblint Executable

To build a Goblint executable with debug information, run the following command within the `analyzer` directory.

`make debug`

This will create a file called `goblint.byte`.

## Debugging Goblint with VS Code

To debug OCaml programs, you can use the command line interface of `ocamldebug` or make use of the Visual Studio Code
integration provided by `hackwaly.ocamlearlybird`.
In the following, we describe the steps necessary to set up this VS Code extension to
debug Goblint.

### Setting-up Earlybird

Install the [`hackwaly.ocamlearlybird` extension](https://marketplace.visualstudio.com/items?itemName=hackwaly.ocamlearlybird) in your installation of Visual Studio Code.
To be able to use this extension, you additionally need to install `ocamlearlybird` on the opam switch you use for Goblint.
Running:

`make dev`

will install `ocamlearlybird` along with some other useful development tools.
In case you do not want to install all of these and only want to install `ocamlearlybird` by itself, run the following command in the `analyzer` directory:

`opam install earlybird`

### Providing a Launch Configuration

To let the `hackwaly.ocamlearlybird` extension know which executable it should debug, and which arguments it should pass, we have to provide a configuration file.
The configuration file has to be named `launch.json` and must reside in the `./.vscode` directory. Here is an example `launch.json`:

```JSON
{
    "version": "0.2.0",
    "configurations": [
      {
        "name": "Goblint",
        "type": "ocamlearlybird",
        "request": "launch",
        "program": "${workspaceFolder}/goblint.byte",
        "arguments": [
          "tests/regression/00-sanity/01-assert.c",
          "--enable", "ana.int.interval",
        ],
        "stopOnEntry": false,
      }
    ]
}
```
Note that the individual arguments to Goblint should be passed here as separate strings that do not contain spaces.

### Running Goblint in the VS Code Debugger

To make sure that VS Code can find `ocamlearlybird`, run the following commands in the `analyzer` directory:

```
eval $(opam env) // Sets up envrionment variables
code .           // Starts VS Code in the current directory
```

After VS Code has started, you can set breakpoints in the Goblint code. You can start debugging using the VS Code command palette with the command `"Debug: Start Debugging"`, or alternatively by pressing `F5`. Note that the Goblint execution is considerably slowed down in the debugger, so you have to be somewhat patient.
Of course, don't forget to rebuild the debuggable executable if you make changes to the code!
