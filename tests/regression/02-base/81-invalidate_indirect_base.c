#include <string.h>
#include <assert.h>

extern void __goblint_unknown(void*); // shallow write

int g;

struct s {
  int *p;
} s = {&g};

int main(void) {
  int *p = s.p;
  __goblint_unknown(&s);
  assert(*p == 0);
  return 0;
}
