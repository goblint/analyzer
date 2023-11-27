#include <string.h>
#include <goblint.h>

struct s {
  int x;
  int *p;
};

int main() {
  int x;
  memset(&x, 0, sizeof(int));
  __goblint_check(x == 0);
  memset(&x, x, sizeof(int));
  __goblint_check(x == 0);
  memset(&x, 1, sizeof(int));
  __goblint_check(x == 0); // UNKNOWN

  int *p;
  memset(&p, 0, sizeof(int*));
  __goblint_check(p == NULL);

  struct s s;
  memset(&s, 0, sizeof(struct s));
  __goblint_check(s.x == 0);
  __goblint_check(s.p == NULL);
  return 0;
}