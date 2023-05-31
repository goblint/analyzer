# Goblint C libraries

Here are C libraries related to Goblint:
1. [`goblint/`](goblint/) — custom Goblint annotations.
2. [`libc/`](libc/) — C standard library and Unix standard libraries.
3. [`sv-comp/`](sv-comp/) — [SV-COMP](https://sv-comp.sosy-lab.org) annotations.
4. [`linux/`](linux/) — Linux kernel libraries.

Each of which may contain two subdirectories:
1. `runtime/` — version of the library for compiling programs.
2. `stub/` — version of the library used by Goblint for analysis.

Each of which has the standard structure:
1. `include` — library header files.
2. `src` — library source files.
