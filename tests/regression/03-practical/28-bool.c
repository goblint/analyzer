#include <stdbool.h>

int main() {
  int a;
  int *p = &a;

  bool x = p; // Unknown int([0,1])
  __goblint_check(x);
  bool y = !!p; // 1
  bool z = p != 0; // 1
  return 0;
}