#include <assert.h>

extern int scanf(char *, ...);

/* pointer backward along the stack to a formal parameter */

void rec (int *p, int i) {
  if (*p)
    return;
  else {
    (*p)++;
    i = (*p);
    p = &i;
    rec(p, i);
    return;
  }
}

main () {
  int a;
  scanf("%d",&a);
  rec(&a, a);
  __goblint_check(a != 0);
}
