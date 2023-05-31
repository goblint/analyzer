// SKIP PARAM: --set ana.activated[+] apron
#include <assert.h>

struct s {
  int x;
  int y;
};

struct s g;

int main() {
  assert(g.x < 10);
  // Manually check that Apron environment doesn't contain just "g" after read from int offset.
  return 0;
}
