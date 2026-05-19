//PARAM: --enable ana.int.interval --set ana.path_sens[+] base
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

int main () {
  int a;
  int b;

  int top;

  if(top) {
    a = 5; b = 5;
  } else {
    a = 10; b = 10;
  }

  __goblint_check(a == b); 
  return 0;
}
