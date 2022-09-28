// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 27-base-unassume-partial-struct-multi.yml
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
  assert(s.i == 2); // UNKNOWN (intentional by unassume)
  assert(0 <= s.j);
  assert(s.j <= 100);
  assert(s.j == 3); // UNKNOWN (intentional by unassume)
  return 0;
}
