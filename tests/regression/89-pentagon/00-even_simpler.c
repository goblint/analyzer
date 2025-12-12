// SKIP PARAM: --conf ./conf/pentagon.json

#include <goblint.h>

void main(void) {
  int x;
  __goblint_check(x >= 0); // UNKNOWN
}
