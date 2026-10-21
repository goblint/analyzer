// SKIP PARAM: --conf ./conf/pentagon.json

#include <goblint.h>

void overwrite_outgoing_relation(void) {
  int x, y, z;
  __goblint_assume(y >= 0 && y <= 10);
  __goblint_assume(z >= 0 && z <= 10);
  __goblint_assume(x < z);
  __goblint_check(x < z); // SUCCESS

  x = y - z;
  __goblint_check(x < z); // UNKNOWN
}

void overwrite_incoming_relation(void) {
  int x, y, z;
  __goblint_assume(y >= -10 && y <= 0);
  __goblint_assume(z >= 0 && z <= 10);
  __goblint_assume(z < x);
  __goblint_check(z < x); // SUCCESS

  x = y - z;
  __goblint_check(z < x); // UNKNOWN
  __goblint_check(x - z > 0); // UNKNOWN
}

int main(void) {
  overwrite_outgoing_relation();
  overwrite_incoming_relation();
}
