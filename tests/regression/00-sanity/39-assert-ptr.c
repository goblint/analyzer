#include <stdlib.h>
#include <assert.h>

int main() {
  int* success = malloc(sizeof(int));
  int* silence = malloc(sizeof(int));
  int* fail = NULL;
  int* unknown;
  // intentionally using assert, specific order to work with assert refine
  assert(success);
  assert(unknown); // UNKNOWN!
  assert(fail); // FAIL!
  return 0;
  assert(silence); // NOWARN!
}
