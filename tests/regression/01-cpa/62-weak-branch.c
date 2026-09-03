#include <stdlib.h>
#include <goblint.h>

void foo(int i, int *p) { // p is actually unused - only used to make x weakly updatable.
  int x[1] = {0};
  // x is an array because in the recursive call initialization is by weak update.
  // If x were just int, then it starts out as top and even the initialization cannot improve it.
  // Arrays start out with bottom contents, so the issue isn't covered up by extreme imprecision.

  if (i == 0) { // initial call from main
    foo(1, &x); // recursive call to just make x weakly updatable.
  }
  else { // recursive call from foo
    int y = 1;

    int r; // rand
    int *q = r ? &x : &y;

    // Spurious branching, which makes x[0] less precise!
    if (*q != 42); // NB! Semicolon - no body for if branches.
    // *q evaluates to [0,1]
    // In true branch:
    //   *q refined to Not{42}
    //   Two cases (joined):
    //     1. x[0] refined to Not{42} - weak update should not make x[0] less precise than 0.
    //     2. y refined to Not{42}

    __goblint_check(x[0] == 0);
  }
}

int main() {
  foo(0, NULL);
  return 0;
}
