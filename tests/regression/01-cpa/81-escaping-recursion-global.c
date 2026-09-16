// A recursive activation's own copy of a local is updated weakly whenever the
// activation can reach an outer copy of it. In 52-escaping-recursion the outer
// copy is reached through an argument; here it is reached through a global.

#include <goblint.h>

static int *outer;

static void f(int n) {
  int x = n;
  if (!outer) {
    outer = &x;
    f(1);
    __goblint_check(x == 2); //UNKNOWN (the inner activation writes only its own x, but one cell carries both)
  } else {
    // `outer` points to the outer x, which is 2: the inner `x = n` wrote the
    // inner copy.
    __goblint_check(*outer == 2); //UNKNOWN (one cell carries both copies of x)
  }
}

int main(void) {
  f(2);
  return 0;
}
