// NONTERM
// PARAM:  --set "ana.activated[+]" localTraces --set solver "WLLocTrac" --set

#include <stdlib.h>
#include <time.h>

void reach_error() {}

void main() {
  srand(time(NULL));
  int x = 3;
  int k = 0;
  while (1) {
    if (1900 < x) {
      x = x + 1900;  // WARN
    } else {
      reach_error();
    }
    x = rand();

    k++;
  }
}