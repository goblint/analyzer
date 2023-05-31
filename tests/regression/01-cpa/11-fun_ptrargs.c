#include<stdio.h>
#include <goblint.h>

struct kala { int x; };
void inc(int *x) {
  (*x)++;
}

void set(int *x, int i) {
  *x = i;
}

void tes(int i, int *x) {
  *x = i;
}

void swap(int *x, int *y) {
  int tmp = *y;
  *y = *x;
  *x = tmp;
}

int main () {
  int i,j;

  i = 0;
  inc(&i);
  __goblint_check(i == 1);

  i = 3; j = 7;
  swap(&i, &j);
  __goblint_check(i == 7);
  __goblint_check(j == 3);

  set(&i, 5);
  __goblint_check(i == 5);

  tes(6, &i);
  __goblint_check(i == 6);

  // struct pointer
  struct kala k;
  k.x = 3;
  inc(&k.x);
  __goblint_check(k.x == 4);

  return 0;
}
