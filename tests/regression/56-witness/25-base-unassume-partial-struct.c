// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 25-base-unassume-partial-struct.yml
#include <assert.h>

struct s {
  int i;
  int j;
};

int main() {
  struct s s;
  s.i = 2;
  s.j = 3;

  assert(1 <= s.i);
  assert(s.i <= 3);
  assert(s.j == 3);
  return 0;
}
