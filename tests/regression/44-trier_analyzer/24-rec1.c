#include <assert.h>

char rec (int x) {
  char v;
  if (x) {
    v = 'a';
    return rec(0);
  } else
    v = 'b';
    return v;
}

main () {
  char c = rec(1);
  __goblint_check(c == 'b');
}
