#include <stddef.h>

int main() {
  int *r; // rand

  if (r == NULL)
    assert(r == NULL);
  else
    assert(r != NULL); // TODO

  if (r != NULL)
    assert(r != NULL); // TODO
  else
    assert(r == NULL);

  return 0;
}
