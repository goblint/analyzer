#include <goblint.h>

extern int scanf(char *, ...);

int *rec(int i) {
  int *p;
  if (!i) {
    __goblint_check(i == 0);
    p = rec(i + 1);
  }
  return &i;
}

main () {
  int *p;
  int i;
  scanf("%d", &i);
  p = rec(i);
}
