// NONTERM
// PARAM: --set ana.activated[+] "localTraces"
#include <stdlib.h>
#include <time.h>

void main() {
  srand(time(NULL));
  int x;
  int y = x;
  int k = 0;
  while (1) {
    if (1900000000 < x) {
      x = x + 2147483647;  // WARN
    }
    if (x < -3) {
      if (-5 < x) {
        // no new value for trace containing x=-4 --> at least one trace stays
        // in loop
      } else {
        x = rand();
      }
    } else {
      x = rand();
    }
    k++;
  }
}