#include<stdio.h>
#include<stdarg.h>
#include<assert.h>

int kala (int x, ...) {
  return x;
}

int main () {
  int i = 0;
  i = kala(3);
  assert(i==3);

  i = kala(2, 5);
  assert(i==2);
  
  return 0;
}
