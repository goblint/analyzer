// PARAM: --disable ana.int.def_exc --enable ana.int.interval --set ana.widen.delay.local 3
// From "Widening as Abstract Domain" slides: https://bytebucket.org/mihaila/bindead/wiki/resources/widening-talk.pdf
// They claim delay 2, we need 3:
// 0. {x -> [0,0], y -> [0,0]}
// 1. {x -> [0,4], y -> [0,0]}
// 2. {x -> [0,8], y -> [0,0]}
// 3. {x -> [0,12], y -> [0,1]} (delay 2 would widen already here)
// 4. {x -> [0,+inf], y -> [0,1]}
// narrow: {x -> [0,103], y -> [0,1]}
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
