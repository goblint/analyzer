// PARAM: --set ana.activated[+] useAfterFree

#include <goblint.h>

static int *escape(void) {
  int local = 42;
  return &local;
}

static int *escape_static(void) {
  static int persistent = 11;
  return &persistent;
}

int main(void) {
  int live = 7;
  int *live_pointer = &live;
  __goblint_check(*live_pointer == 7);

  int *dangling = escape();
  int value = *dangling; // WARN (automatic storage ended when escape returned)
  __goblint_check(value == 42); // UNKNOWN

  int *persistent = escape_static();
  __goblint_check(*persistent == 11); // Static storage survives return.
  return 0;
}
