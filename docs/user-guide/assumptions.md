# Assumptions

Goblint makes the following (implicit) assumptions about the systems and programs it analyzes.

_NB! This list is likely incomplete._

1. `PTHREAD_MUTEX_DEFAULT` is a non-recursive mutex type.

    Although the [POSIX Programmer's Manual](https://linux.die.net/man/3/pthread_mutexattr_settype) states that

    > An implementation may map this mutex to one of the other mutex types.

    including `PTHREAD_MUTEX_RECURSIVE`, on Linux it seems to be mapped to `PTHREAD_MUTEX_NORMAL`.
    Goblint assumes this to be the case.

    This affects the `maylocks` analysis.

    See [PR #1414](https://github.com/goblint/analyzer/pull/1414).

2. Implementation-defined behavior usually follows GCC.

    For example, for the behavior on integers, Goblint assumes the compiler behaves as documented by [GCC](https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html). This is also in line with the behavior of Clang in this case.

    > > The result of, or the signal raised by, converting an integer to a signed integer type when the value cannot be represented in an object of that type (C90 6.2.1.2, C99 and C11 6.3.1.3).
    >
    > For conversion to a type of width N, the value is reduced modulo 2^N to be within range of the type; no signal is raised.
