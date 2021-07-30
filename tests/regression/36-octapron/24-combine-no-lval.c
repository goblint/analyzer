// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

void foo() {

}

int main(void) {
  int x, y, r;

  if (r) {
    x = 3;
    y = 5;
  }
  else {
    x = 4;
    y = 6;
  }

  assert(x < y);
  assert(y - x == 2);

  foo(); // combine without lval shouldn't ruin local state

  assert(x < y);
  assert(y - x == 2);
  return 0;
}
