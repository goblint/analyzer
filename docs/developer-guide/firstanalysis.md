# Your first analysis

This is a tutorial exercise for writing analyses in Goblint.
We will implement a very simple sign analysis.
You need to install Goblint for [development](../developer-guide/developing.md).

## First test

We will analyze the following C program.
It is not the most complicated program one could imagine, but we have to start somewhere.

```c
#include <assert.h>

int main() {
  int x;
  int unknown;

  if (unknown) {
    x = -5;
  } else {
    x = -7;
  }

  // The above code branches on an uninitialized variable.
  // The value of x could be either -5 or -7.

  assert(x < 0); // TODO: Thus, this assertion should hold!

  return 0;
}
```

This program is in the Goblint repository: `tests/regression/99-tutorials/01-first.c`.
But if you run Goblint out of the box on this example, it will not work:

```console
./goblint --enable warn.debug tests/regression/99-tutorials/01-first.c
```

This will claim that the assertion in unknown.
Goblint could verify that this assertion does hold using interval analysis (`--enable ana.int.interval`), but here we will implement a simple sign analysis instead.

## Starting point

We begin with the flawed implementation in **`src/analyses/tutorials/signs.ml`**.
If you immediately try to run Goblint with the new analysis enabled: `--set "ana.activated[+]" signs`. The result will still be that nothing is verified, so you need to fix all the problems in the code.

It may still be useful to use Goblint's HTML output to [see the result](../user-guide/inspecting.md) of the analysis. This will also include Goblint's base analysis, which is needed to deal with function calls.

## Designing the domain

We first need to design the abstract domain. It may help if you have read some theoretical tutorial on abstract domains. Our first sign lattice will simply contain the elements `{-, 0, +}` with top and bottom added. These elements are defined in the module `Signs` and then we define the sign lattice `SL` by adding bottom and top elements. This is done by the functor `Lattice.Flat`. You should look at the following functions and fix their problems.

1. `of_int i` should abstract integers to their best representation in our abstract domain. Our sign domain can distinguish positive, negative and zero values, so do it right!
2. `lt x y` should answer true if the value represented by `x` is definitely less than the value represented by `y`. There seems to be a crucial case missing here in the otherwise excellent implementation...

We will represent the abstract state of the program as a map from variables to the newly created sign domain.

```ocaml
module D = MapDomain.MapBot (Basetype.Variables) (SL)
```

## Implementing the sign analysis

The key part now is to define transfer functions for assignment. We only handle assignments of the form `x = e` where`x` is variable whose address is never taken and the right-hand side `e` is itself either a constant of type integer or a plain variable.
There is no need to implement the transfer functions for branching for this example; it only relies on lattice join operations to correctly take both paths into account.

The assignment relies on the function `eval`, which is almost there. It just needs you to fix the evaluation of constants! Unless you jumped straight to this line, it should not be too complicated to fix this.
With this in place, we should have sufficient information to tell Goblint that the assertion does hold.


## Extending the domain

You could now enrich the lattice to also have a representation for non-negative (i.e., zero or positive) values.
Then the join of `Zero` and `Pos` would be "non-negative" instead of `Top`, allowing you to prove that such join is greated than `Neg`.
For example, have a look at the following program: `tests/regression/99-tutorials/02-first-extend.c`.

_Hint:_
The easiest way to do this is to use the powerset lattice of `{-, 0, +}`.
For example, "non-negative" is represented by `{0, +}`, while negative is represented by `{-}`.
To do this, modify `SL` by using `SetDomain.FiniteSet` (takes a `struct` with a list of finite elements as second parameter) instead of `Lattice.Flat` and reimplementing the two functions using `singleton` and `for_all`.
