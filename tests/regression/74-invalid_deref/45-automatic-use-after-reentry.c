// PARAM: --set ana.activated[+] useAfterFree

#include <goblint.h>

static int *escaped;

static void activation(int retain) {
  int local = 42;
  if (retain)
    escaped = &local;
  else {
    int value = *escaped; // WARN (points to the preceding, returned activation)
    __goblint_check(value == 42); // Base still tracks the value despite undefined access.
  }
}

int main(void) {
  activation(1);
  activation(0);
  return 0;
}
