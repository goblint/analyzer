#include <assert.h>

typedef struct {
  int x;
} a;

int main() {
  a z;
  a *y = &z;

  int *m = &y->x; // {&z.x}
  a *n = &y[0]; // {&z[def_exc:0]}

  int b = m == n;
  assert(b); // UNKNOWN
  return 0;
}
