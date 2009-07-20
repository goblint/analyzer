#include <stdlib.h>
#include <assert.h>

int main() {
  int* x[10];
  int i = 0;

  while (i < 10)
    x[i++] = malloc(sizeof(int));

  *x[3] = 50;
  *x[7] = 100;
  assert(*x[8] == 100); // UNKNOWN

  return 0;
}
