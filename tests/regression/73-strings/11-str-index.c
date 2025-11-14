#include <goblint.h>

int main() {
  char *s = "abc";
  int x = s[0];
  __goblint_check(x == 97);
  return 0;
}
