// PARAM: --disable ana.int.def_exc --enable ana.int.interval --set ana.widen.delay.local 3
// From "Widening as Abstract Domain" slides: https://bytebucket.org/mihaila/bindead/wiki/resources/widening-talk.pdf
// They claim delay 2, we need 3 for some reason. Why?
#include <goblint.h>

int main() {
  int x = 0;
  int y = 0;
  while (x < 100) {
    __goblint_check(0 <= y);
    __goblint_check(y <= 1);
    if (x > 5)
      y = 1;
    x = x + 4;
  }
  __goblint_check(0 <= y);
  __goblint_check(y <= 1);
  return 0;
}
