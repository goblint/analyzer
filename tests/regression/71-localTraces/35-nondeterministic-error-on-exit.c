// NONTERM
// PARAM: --set ana.activated[+] "localTraces"
#include <stdlib.h>
#include <time.h>

int k = 0;

void main() {
  srand(time(NULL));
  int x;
  int y = x;
  while (k < 1) {
    if (1900000000 < x) {
      k = 42;
    }
    x = rand();
  }
  x = x + 2147483647;  // WARN
}