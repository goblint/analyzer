#include <stdlib.h>

extern void *malloc(size_t);
extern int scanf(char *, ...);

/* pointer forward along the stack to a formal parameter */

void rec (int **u, int i) {
  *u = &i;
  if (i)
    return;
  else {
    i++;
    u = (int **)malloc(sizeof(int *));
    int** v = u;
    rec(u, i);
    __goblint_check(v == u); //TODO
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
