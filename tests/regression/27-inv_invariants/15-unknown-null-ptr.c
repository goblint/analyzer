#include <stddef.h>

int main() {
  int *r; // rand

  if (r == NULL)
    assert(r == NULL);
  else
    assert(r != NULL);

  if (r != NULL)
    assert(r != NULL);
  else
    assert(r == NULL);

  return 0;
}
