//SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>
#include <goblint.h>

int main() {
  int x = 0;
  int y = 0;

  x = 1;
  y = 1;

  __goblint_check(x == y); //SUCCESS

  x = 10;

  __goblint_check(x != y); //SUCCESS
  __goblint_check(x == y); //FAIL

  return 0;
}

