//SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>
#include <goblint.h>

int main() {
  int arr[2] = {10, 20};
  int x = arr[0];
  int y = arr[1];

  __goblint_check(x!=y); //SUCCESS

  arr[0] = 20;

  __goblint_check(x != y); //SUCCESS
  __goblint_check(x==y); //FAIL

  return 0;
}