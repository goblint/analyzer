// SKIP PARAM: --set ana.activated[+] apron

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
