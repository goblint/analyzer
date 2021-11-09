#include<stdio.h>
#include<assert.h>

int x = 0;

int g(int a) {
  return a * a;
}

int f() {
  return g(x) + 2;
}

void callme(void)  {
  x = 5;
  int y = f() + x;
  assert(y == 32);
}

void callfun(void (*fun)()) {
  fun();
  return;
}

int main() {
  callfun(& callme);
  assert(x == 5);
  int z = g(3);
  return 0;
}
