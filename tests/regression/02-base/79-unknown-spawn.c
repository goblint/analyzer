// PARAM: --enable sem.unknown_function.spawn
#include <goblint.h>
#include <stddef.h>

int magic(void* (f (void *)));

void *t_fun(void *arg) {
  // __goblint_check(1); // reachable
  return NULL;
}

int main() {
  magic(t_fun); // unknown function
  return 0;
}
