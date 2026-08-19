// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

struct box {
  int *item;
};

// Neither function has a definition or a library specification, so each may
// free any heap object reachable from its argument.
extern void dispose(int *p);
extern void dispose_box(struct box b);

int main(void) {
  int *p = malloc(sizeof(int));
  *p = 1; // NOWARN
  dispose(p);
  *p = 2; // WARN

  // An argument of struct type passes the pointers it holds.
  struct box b;
  b.item = malloc(sizeof(int));
  *b.item = 1; // NOWARN
  dispose_box(b);
  *b.item = 2; // WARN
  return 0;
}
