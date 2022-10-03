// From the Trier benches...

#include<stdio.h>
#include <goblint.h>

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
  __goblint_check(x == 5);
  return 0;
}
