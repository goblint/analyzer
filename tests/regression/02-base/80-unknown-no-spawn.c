// PARAM: --disable sem.unknown_function.spawn
#include <assert.h>
#include <stddef.h>

void *t_fun(void *arg) {
  assert(1); // NOWARN (unreachable)
  return NULL;
}

int main() {
  magic(t_fun); // unknown function
  return 0;
}
