// PARAM: --set ana.activated[+] expsplit
#include <stddef.h>
#include <goblint.h>

int main() {
  int r; // rand
  int x, y;
  int *p;

  __goblint_split_begin(x);
  if (r) {
    x = 1;
    p = &y;
  }
  else {
    x = 2;
    p = NULL;
  }

  __goblint_check((x == 1 && p == &y) || (x == 2 && p == NULL));

  __goblint_split_end(x);

  __goblint_check((x == 1 && p == &y) || (x == 2 && p == NULL)); // UNKNOWN (intentionally)

  return 0;
}