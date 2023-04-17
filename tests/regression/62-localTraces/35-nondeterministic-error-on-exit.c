// PARAM: --set ana.activated[+] "localTraces"
#include <stdlib.h>
#include <time.h>

void main() {
  srand(time(NULL));
  int x;
  int y = x;
  int k = 0;
  while (k < 1) {
    if (1900000000 < x) {
      k = 42;
    }
    x = rand();
  }
  x = x + 2147483647;  // WARN
}