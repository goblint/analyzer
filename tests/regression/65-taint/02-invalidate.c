//PARAM --set "ana.activated[+]" taintPartialContexts --set ana.ctx_insens[+] base
#include <goblint.h>

extern void unknown_fun (int *a);

void mainfunct(int *rptr, int* uptr) {
  unknown_fun(rptr);
}

int g;

int main() {
  int r, u;

  g = 1;
  r = 1;
  u = 1;
  mainfunct(&r, &u);

  g = 2;
  r = 2;
  u = 2;
  mainfunct(&r, &u);

  __goblint_check(g == 2); //UNKNOWN!
  __goblint_check(r == 2); //UNKNOWN!
  __goblint_check(u == 2);
}
