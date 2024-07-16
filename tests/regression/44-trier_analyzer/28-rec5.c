#include <goblint.h>

extern int scanf(char *, ...);

/* both backward and forward pointers along the stack to local variables */

void rec (int **u, int i) {
  *u = &i;
  if (i)
    return;
  else {
    int *a;
    i++;
    u = &a;
    rec(u, i);
    __goblint_check(u == &a); // Holds here, we are not dereferencing u
    return;
  }
}

main () {
  int a;
  int *p;
  p = &a;
  scanf("%d", p);
  rec(&p, a);
}
