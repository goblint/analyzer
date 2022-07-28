#include <assert.h>
#include<stdbool.h>

int main() {
  int x;
  bool y;
  x = 4;
  y = true;
  __goblint_check(y);
  return x;
}
