#include<stdio.h>
#include<assert.h>

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
  assert(i == 1);

  i = 3; j = 7;
  swap(&i, &j);
  assert(i == 7);
  assert(j == 3);
  
  set(&i, 5);
  assert(i == 5);

  tes(6, &i);
  assert(i == 6);

  // struct pointer
  struct kala k;
  k.x = 3;
  inc(&k.x);
  assert(k.x == 4);

  return 0;
}
