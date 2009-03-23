// SKIP!
#include<stdio.h>
#include<assert.h>

int t;

void rec1 (int x) {
  if (x) {
    t = 3;
    rec1(0);
  } else
    t = 5;
}

void rec2 (int *p, int x) {
  int i = 0;
  // the first call
  if (x == 0) { 
    rec2(&i, 1);
    *p = i;
  // the second call
  } else { 
    *p = 9;
    i = 0;
  }
}

int fact(int x) {
  if (x < 2) 
    return 1;
  else 
    return x * fact (x-1);
}

int main () {
  int a = 1;
  
  rec1(0);
  assert(t == 5);

  rec2(&a, 0);
  printf("a = %d\n", a);
  assert(a == 9);

  a = fact(6);
  assert(a == 720);

  return 0;
}
