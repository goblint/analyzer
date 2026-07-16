// PARAM: --set ana.base.strings.domain unit
#include <string.h>
#include <goblint.h>

void foo(char *s) {
  int l = strlen(s);
  __goblint_check(l == 3 || l == 6); // UNKNOWN
}

int main() {
  foo("foo");
  foo("bar");
  foo("foobar");
  return 0;
}
