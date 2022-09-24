// PARAM: --set ana.activated[+] expsplit
#include <stddef.h>
#include <assert.h>
#include <goblint.h>

int i(int x) {
  __goblint_split_begin(x);
  return x;
}

int main() {
  int r; // rand
  int x, y;
  int *p;

  if (r) {
    p = &y;
    x = i(1); // combine last before join to check if combine splits
  }
  else {
    p = NULL;
    x = i(2); // combine last before join to check if combine splits
  }

  __goblint_check((x == 1 && p == &y) || (x == 2 && p == NULL));

  __goblint_split_end(x);

  __goblint_check((x == 1 && p == &y) || (x == 2 && p == NULL)); // UNKNOWN (intentionally)

  return 0;
}