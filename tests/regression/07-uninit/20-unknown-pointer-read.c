// PARAM: --set ana.activated[+] uninit

#include <stdint.h>

struct reply { int tag; int value; };

static int partial(void) {
  struct reply reply;
  reply.tag = 1;
  // The pointer-to-integer round trip loses the concrete target in Base.
  uintptr_t bits = (uintptr_t)&reply;
  struct reply *p = (struct reply *)bits;
  return p->value; // WARN
}

static int initialized(void) {
  struct reply reply = {1, 42};
  uintptr_t bits = (uintptr_t)&reply;
  struct reply *p = (struct reply *)bits;
  return p->value; // NOWARN
}

int main(void) {
  initialized();
  return partial();
}
