# Assumptions

An analysis that cannot establish a property may proceed by assuming it. A result computed that way holds only if the assumption holds of the program being analyzed, so whether an assumption is acceptable is the reader's decision, not the analyzer's.

## Reported assumptions

Some assumptions are reported by the run that uses them, through `Assumptions.add` (`src/common/util/assumptions.ml`): once at each location where the assumption changed what the analysis did, and once at the end of the run, naming the assumption however many locations used it. Both messages carry the category `Assumption` at severity `Info`.

An `Assumption` message is not a warning about the analyzed program — it says a result is conditional — so a count of alarms should exclude it. Filter on the category rather than the severity, which `Info` shares with other messages: in `--set result json-messages` the tag is `{"Category": ["Assumption"]}`. `--disable warn.assumption` silences both messages. The number of locations an assumption was used at is the number of its located messages, which the closing line does not repeat.

An assumption that a run never used prints nothing, so the list at the end of a run is what that run's results actually depend on.

The model of the C library's standard streams (`src/util/library/standardStreams.ml`) names one assumption the same way:

| Identifier | What is assumed |
| --- | --- |
| `libc.stream-buffer-lifetime` | A buffer attached to a stream with `setvbuf` or `setbuf` stays live until the stream is closed or the program exits. |

The analysis tracks the buffer of the standard streams `stdin`, `stdout` and `stderr` only, and most ends of such a buffer's storage are checked rather than assumed: base reports a `UseAfterFree` warning (CWE-562 for a local, CWE-416 otherwise) where a function returns, or a `longjmp` leaves it, while a standard stream holds one of its locals as its buffer; where heap memory a standard stream holds is freed or reallocated, or where a pointer whose target is not known is freed or reallocated while a standard stream holds heap memory; where any thread, `main`'s included, calls `pthread_exit` while a standard stream holds a local of any function or thread-local storage; where `pthread_cancel` is called while a standard stream holds a local of any function; and where a thread other than the main one returns while a standard stream holds thread-local storage. At `pthread_exit` and `pthread_cancel` the frames on the exiting or cancelled thread's stack are not known, so the warning covers every local a stream holds, and a local of a function on another thread's stack is a false alarm. A local of `main` is left out where the exiting or cancelled thread is known not to be the main one and the program neither calls `main` nor takes its address, since `main` then has frames only on the main thread's stack. `exit` needs no check: it flushes the streams before any frame ends. Returning from `main` is one of these: it ends `main`'s local variables before `exit` flushes and closes the streams (C11 5.1.2.2.3 and 7.22.4.4). `fclose` detaches the buffer; `setvbuf(stream, NULL, ...)` does too, although `setvbuf` may be called only before any other operation on the stream (C11 7.21.5.6), so a buffer that must end before the program does is only correct when the stream is closed first.

The assumption is reported at a `setvbuf`, `setbuf` or `setbuffer` whose buffer may be storage whose end those checks may not see. For a stream that may be other than a standard stream — one `fopen` returned, `stdout` once the program assigned it another stream, or a stream the analysis does not know — that is every buffer except static storage, heap memory included, since no end of that stream's buffer is checked. For a standard stream it is an unknown address; `alloca` memory; thread-local storage, whose thread may be cancelled; a local that may be declared in a nested block, which ends without a return (it is not reported for a local of a function's outermost block when `cil.addNestedScopeAttr` is enabled); a variable-length array, even in a function's outermost block, whose lifetime also ends where a `goto` jumps back before its declaration (C11 6.2.4p7); any local, when the program calls `pthread_cancel` or takes its address, since a thread acts on a cancellation request later — at a cancellation point, or at any instruction under asynchronous cancellation — and so may end with a buffer attached after the request, which the check at `pthread_cancel` does not see. A static buffer needs none, and neither does heap memory held by a standard stream.

There is no option to turn any of these off. An option would earn its place only where the analysis, with the option unset, computes a sound over-approximation instead of the assumed result; an option whose unset position merely prints the unsoundness does not qualify, because reporting is what `Assumptions.add` already does, always.

## Implicit assumptions

Goblint additionally makes the following assumptions about the systems and programs it analyzes, which a run does not report.

_NB! This list is likely incomplete._

1. `PTHREAD_MUTEX_DEFAULT` is a non-recursive mutex type.

    Although the [POSIX Programmer's Manual](https://linux.die.net/man/3/pthread_mutexattr_settype) states that

    > An implementation may map this mutex to one of the other mutex types.

    including `PTHREAD_MUTEX_RECURSIVE`, on Linux and OSX it seems to be mapped to `PTHREAD_MUTEX_NORMAL`.
    Goblint assumes this to be the case.

    This affects the `maylocks` analysis.

    See [PR #1414](https://github.com/goblint/analyzer/pull/1414).

2.  Pointer arithmetic does not overflow.

    [C11's N1570][n1570] at 6.5.6.8 states that

    > When an expression that has integer type is added to or subtracted from a pointer, the result has the type of the pointer operand.
    > [...]
    > the evaluation shall not produce an overflow; otherwise, the behavior is undefined.

    after a long list of defined behaviors.

    Goblint does not report overflow and out-of-bounds pointer arithmetic (when the pointer _is not dereferenced_).
    This affects the overflow analysis (SV-COMP no-overflow property) in the `base` analysis.

    This _does not_ affect the `memOutOfBounds` analysis (SV-COMP valid-memsafety property), which is for undefined behavior from _dereferencing_ such out-of-bounds pointers.

    See [PR #1511](https://github.com/goblint/analyzer/pull/1511).

3.  Memory allocated with `malloc` is initialized before reading.

    [C11's N1570][n1570] at J.2 states that

    > The behavior is undefined in the following circumstances:
    >
    > - [...]
    > - The value of the object allocated by the `malloc` function is used (7.22.3.4).

    after a long list of undefined behaviors.

    Goblint does not report reading from uninitialized memory allocated by `malloc`.

    This affects the `base` analysis.

    See [issue #1770](https://github.com/goblint/analyzer/issues/1770).

4.  Array index reads are in bounds.

    [C11's N1570][n1570] at J.2 states that

    > The behavior is undefined in the following circumstances:
    >
    > - [...]
    > - An array subscript is out of range, even if an object is apparently accessible with the given subscript (as in the lvalue expression `a[1][7]` given the declaration `int a[4][5]`) (6.5.6).

    after a long list of undefined behaviors.

    Goblint does not by default report reading from out of bounds array indices.
    Goblint reports out of bounds array index accesses when the `ana.arrayoob` option is enabled.

    This affects the `base` analysis.

    See [issue #1702](https://github.com/goblint/analyzer/issues/1702).


[n1570]: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
