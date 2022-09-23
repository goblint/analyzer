// SKIP!
#include<stdio.h>
#include<assert.h>

int *dang() {
  int a = 3;
  return &a;
}

void nestra(int **p) {
  int a;
  a = 5;
  *p = &a;
}

int main () {
  int i, *p, *q;

  // dangling pointer stuff
  p = dang();
  *p = 7;
  i = *p;
  printf("%d\n", i); // GCC: i = 7
  __goblint_check(i == 7);

  dang();
  i = *p;
  printf("%d\n", i); // GCC: i = 3
  __goblint_check(i == 3); // UNKNOWN

  nestra(&p);
  *p = 8;
  nestra(&q);
  i = *p;
  printf("%d\n", i); // GCC: i = 5
  __goblint_check(i == 5); // UNKNOWN

  return 0;
}
