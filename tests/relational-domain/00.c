#include <stdio.h>
#include <goblint.h>

int main() {
  int x = 0;
  int y = 0;

  x = 1;
  y = 1;

  __goblint_check(x == y);

  x = 2;

  __goblint_check(x != y);

  return 0;
}