#include <stdio.h>
#include <goblint.h>

int main() {
  int arr[2] = {10, 20};
  int x = arr[0];
  int y = arr[1];

  printf(x);

  __goblint_check(x!=y); //UNKNOWN!

  arr[0] = 20;

  __goblint_check(x != y); //UNKNOWN!
  __goblint_check(x==y); //UNKNOWN!

  return 0;
}