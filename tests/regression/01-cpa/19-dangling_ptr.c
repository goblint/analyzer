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
  q = dang();
  i = *q;
  assert_unknown(i);
  printf("%d\n", i);

  nestra(&p);
  *p = 8;
  nestra(&q);
  i = *q;
  assert_unknown(i);
  printf("%d\n", i);

  return 0;
}
