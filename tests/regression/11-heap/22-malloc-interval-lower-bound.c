// PARAM: --set ana.malloc.unique_address_count 2
#include <goblint.h>
#include <stdlib.h>

static int *alloc(void) {
  return malloc(sizeof(int));
}

int main(int argc, char **argv) {
  int *first = alloc();
  *first = 42;
  if (argc > 1)
    alloc();

  // Counter [1,2]: the next allocation can be #1 or summary, but never #0.
  int *next = alloc();
  *next = 0;
  __goblint_check(*first == 42);
  return 0;
}
