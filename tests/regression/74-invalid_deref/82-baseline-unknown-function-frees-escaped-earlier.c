// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

struct holder {
  int *item;
};

// Neither function has a definition or a library specification: register_holder
// may keep the address of h, and release_all may then free what h points to,
// although it takes no argument and no global reaches h.
extern void register_holder(struct holder *h);
extern void release_all(void);

int main(void) {
  struct holder h;
  h.item = NULL;
  register_holder(&h);
  h.item = malloc(sizeof(int));
  *h.item = 1; // NOWARN
  int *q = malloc(sizeof(int));
  *q = 1; // NOWARN
  release_all();
  *h.item = 2; // WARN
  *q = 2; // NOWARN (no unspecified function can reach q)
  return 0;
}
