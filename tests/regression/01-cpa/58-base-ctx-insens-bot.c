// PARAM: --set ana.ctx_insens[+] base
#include <stdlib.h>
#include <assert.h>

void foo(int cond) {
  assert(cond); // TODO to match assert
}

int main() {
  int *p = malloc(sizeof(int)); // blob is initially Bot
  while (1) { // blob joins Bot and 42 -> 42
    // TODO: both should actually be UNKNOWN! to be sound
    assert(*p == 42);
    foo(*p == 42); // Bot -> Top, so foo has unknown and cannot narrow
    *p = 42; // blob becomes 42
  }
  return 0;
}
