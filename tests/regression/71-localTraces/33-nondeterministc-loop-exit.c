// NONTERM
// PARAM:  --set "ana.activated[+]" localTraces --set solver "WLLocTrac" --set
// warn_at "early" --set verify false
#include <stdlib.h>
#include <time.h>

void main() {
  srand(time(NULL));
  int x = 3;
  int k = 0;
  while (1) {
    if (1900000000 < x) {
      x = x + 2147483647;  // WARN
    }
    x = rand();

    k++;
  }
}