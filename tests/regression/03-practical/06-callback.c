// From the Trier benches...

#include<stdio.h>
#include<assert.h>

int x = 0;

void callme(void)  {
  x = 5;
}

void callfun(void (*fun)()) {
  fun();
  return;
}

int main() {
  callfun(& callme);
  assert(x == 5);
  return 0;
}
