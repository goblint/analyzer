#include <string.h>
#include <assert.h>

struct s {
  int x;
  int *p;
};

int main() {
  int x;
  memset(&x, 0, sizeof(int));
  assert(x == 0);
  memset(&x, x, sizeof(int));
  assert(x == 0);
  memset(&x, 1, sizeof(int));
  assert(x == 0); // UNKNOWN

  int *p;
  memset(&p, 0, sizeof(int*));
  assert(p == NULL);

  struct s s;
  memset(&s, 0, sizeof(struct s));
  assert(s.x == 0);
  assert(s.p == NULL);
  return 0;
}