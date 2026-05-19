//PARAM: --enable ana.int.interval --set ana.activated[+] branchSet
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

int main () {
  int a;
  int b;

  int top;
  int top2;

  if(top) {
    a = 5; b = 5;
  } else {
    a = 10; b = 10;
  }

  if(top2) {
    a = -5; b = 8;
  }

  __goblint_check(top2 || a == b); 
  return 0;
}
