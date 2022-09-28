// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 26-base-unassume-partial-struct-mem.yml
#include <assert.h>

struct s {
  int i;
  int j;
};

int main() {
  struct s s;
  s.i = 2;
  s.j = 3;
  int *p = &s.i;
  assert(1 <= s.i);
  assert(s.i <= 3);
  assert(s.j == 3);
  assert(s.i == 2); // UNKNOWN (intentional by unassume)
  return 0;
}
