// Current running command:
// ./goblint tests/regression/86-pentagon/01-pentagon-simple.c --conf ./conf/pentagon.json

#include <goblint.h>

void main(void) {
  int x;
  __goblint_check(x >= 0); // UNKNOWN
}
