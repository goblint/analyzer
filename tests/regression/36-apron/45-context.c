// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval --enable ana.apron.context
#include <assert.h>

int oct(int x, int y) {
  int s;
  if (x <= y)
    s = 1;
  else
    s = 0;
  return s;
}

void main() {
  int x, y, res;
  if (x <= y) {
    res = oct(x, y);
    assert(res == 1);
  }

  res = oct(x, y);
}
