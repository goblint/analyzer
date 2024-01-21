// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <limits.h>
#include <stdio.h>

int x;
int y;

void setY() { y = x + 3; }

int main() {
  int k;
  x = k * k;

  if (x < INT_MAX - 3) {
    setY();

    __goblint_check(y == x + 3); // SUCCESS
  }

  return 0;
}
