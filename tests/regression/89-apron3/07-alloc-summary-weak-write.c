// SKIP PARAM: --set ana.activated[+] apron

#include <goblint.h>
#include <stdlib.h>

static int *make(int v) {
  int *p = malloc(sizeof(int));
  *p = v;
  return p;
}

int main(void) {
  int *a = make(3);
  int *b = make(7);
  __goblint_check(*a == 3); // UNKNOWN (one variable per allocation site carries both blocks)
  __goblint_check(*b == 7); // UNKNOWN (the same variable)
  return 0;
}
