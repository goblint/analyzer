// PARAM: --set ana.activated[+] apron

// `f(2, 0)` calls `f(1, &x)`, so while the inner activation runs two copies of
// `f`'s local `x` are live, one relational variable stands for both, and
// `Queries.IsMultiple` holds of it. The inner activation's `x == 1` is a fact
// about its own copy; refining the variable by it would also make the outer
// copy 1, and `*parent` reads the outer copy. `*parent == 2` holds in every
// execution that reaches either assertion.

#include <goblint.h>

static void f(int n, int *parent) {
  int x = n;
  if (parent) {
    __goblint_check(*parent == 2); // UNKNOWN (one variable carries both copies of x)
    if (x == 1) {
      __goblint_check(*parent == 2); // UNKNOWN (the condition on the inner copy must not refine the outer one)
    }
  } else {
    f(1, &x);
  }
}

int main(void) {
  f(2, 0);
  return 0;
}
