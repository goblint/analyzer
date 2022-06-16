# Extending library

Analyzed programs often call functions for which the implementation is not available to Goblint.
These can belong to the standard libraries of C, POSIX, Linux kernel, etc or to project dependencies whose implementation is not available.

Goblint outputs "Function definition missing" errors in such cases and by default assumes the worst about what the function does (invalidating everything).
Such invalidation is a major source of imprecision and should be avoided at all cost (while still being sound).
The semantics of unknown functions are controlled by options under `sem.unknown_function`, which can be used to avoid the invalidation, but disabling them if any unknown function does such invalidation will lead to unsoundness.

To strike a better balance between soundness and precision, more fine-grained specifications can be provided in the `LibraryFunctions` module.


## Library function specifications

In the `LibraryFunctions` module (implementation), specifications of library functions are organized into a number of association lists.
Functions are grouped based on their origin and context.
This (in the future[^spec-list-selection]) allows groups of library function specifications to be toggled based on the program under analysis.

There are also older (less granular) specifications implemented using `classify` and `invalidate_actions`.
No new specifications should be added there and all the existing ones should be migrated to the new DSL-based format.

[^spec-list-selection]: Specification list toggling will only be possible when all the old specifications are migrated.


## Library function descriptor DSL

A library function's specification is a pair of its name and descriptor (of type `LibraryDesc.t`).
The descriptors are easiest written using the OCaml DSL from `LibraryDsl`.


### Function descriptor

A function descriptor can be one of the following:

1. `special` consists of an arguments descriptor and a continuation function for converting the captured arguments to a `LibraryDesc.special` variant.
    This is used when the library function should (or at least could) be specially handled by analyses.

2. `unknown` consists of just an arguments descriptor (which must `drop` all arguments) and defaults to `Unknown` variant.
    This is used when the library function isn't specially handled by analyses.


### Arguments descriptor

An arguments descriptor is a list (with standard OCaml list syntax) of individual argument descriptors.
Additionally for library functions with variadic arguments, the list may be terminated with `VarArg` constructor instead of `[]`.


### Argument descriptor

An argument descriptor can be one of the following:

1. `__` captures the argument expression for use in `special`'s continuation function.
2. `drop` ignores (drops) the argument expression to not be used by the continuation.

Both functions consist of an argument name string (for readability purposes) and a list of accesses the function does to the _pointer_ argument.

Unnamed variants of the functions `__'` and `drop'` also exist, but should be avoided.


### Access

An access consists of an access kind and depth.
Access kinds (`AccessKind.t`) are: read, write, free and spawn.

Accesses specify what the library function may do with the corresponding _pointer_ argument.
Since function calls unconditionally read the immediate expressions given as arguments, there would be no point specifying that all the arguments (including of integer) type are read.
Also, non-pointer arguments are copied (passed-by-value) to the function, so write accesses would make no sense.
Therefore only pointer arguments should have accesses specified.

Two depths of accessing are distinguished:

1. Shallow (non-transitive, may-point-to) access means the function dereferences the pointer argument, but doesn't dereference any pointers within (e.g. if a pointer to struct is given as argument, but the struct contains further pointers).
    This is is almost always the case for standard library functions, since they take `void*` or `char*` as generic memory arguments and don't know about any pointers within the pointed block of memory.

2. Deep (transitive, reachable) access means the function follows (dereferences) pointers within.

In the DSL, shallow accesses can be specified by `r`, `w`, `f` or `s`, respectively. Deep accesses can be specified using the previous values with an additional `_deep` suffix.


