#include <assert.h>

extern int printf (char *, ...);

void *awful(int **u) {
  int a;
  a = 5;
  (*u) = &a;
}

main () {
  int *p;
  awful(&p);
  __goblint_check(*p == 5); //UNKNOWN!
  printf("%d\n", *p);
}
