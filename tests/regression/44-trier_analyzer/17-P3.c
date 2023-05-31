#include <stdlib.h>

extern void *malloc(size_t);
extern int printf();

int proc (int **x, int **y) {
  int *z;
  z = *x;
  *x = *y;
  *y = z;
  return 2;
}

main () {
  int *a, *b, *p, *q;
  a = (int *)malloc(sizeof(int));
  b = (int *)malloc(sizeof(int));
  (*a) = 0;
  (*b) = 1;
  p = a;
  q = b;
  printf("Before  proc :\n");
  printf("*a = %d, *b = %d,\n",*a,*b);
  printf("*p = %d, *q = %d.\n",*p,*q);
  *p = proc(&p, &q);
  printf("After  proc :\n");
  printf("*a = %d, *b = %d,\n",*a,*b);
  printf("*p = %d, *q = %d.\n",*p,*q);
}