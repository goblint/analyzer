// NONTERM
// PARAM: --set ana.activated[+] "localTraces"
#include <stdlib.h>
#include <time.h>

int k = 0;

void main() {
  srand(time(NULL));
  int x;
  int y = x;
  while (k < 43) {
    while (k < 1) {
      if (1800000000 < x) {
        k = 42;
      }
      x = rand();
    }
    x = rand();
    if (1500000000 < x) {
      k = 50;
    }
  }
  x = x + 2147483647;  // WARN
}