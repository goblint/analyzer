// CRAM
#include <goblint.h>

int g;

int main() {
  __goblint_check(g == 0);
  g = 1;
  __goblint_check(g == 1);
  return 0;
}
