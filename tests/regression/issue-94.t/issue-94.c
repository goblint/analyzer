#include <assert.h>

int main() {
  int x;
  if (1)
    x = 1;
  else
    x = 2;
  if (x)
    x = 1;
  else
    x = 2;
  assert(x > 1 && x < 0);
}
