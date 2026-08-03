// PARAM: --set ana.activated[+] uninit
#include <goblint.h>
static int read_outer(int depth, int *outer) {
  int x;
  if (depth) return read_outer(0, &x);
  x = 1;
  return *outer; // WARN
}
int main(void) { return read_outer(1, 0); }
