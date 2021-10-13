// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

int g;
int h;

void f(int x, int y) {
  assert(x < y);
}

int main(void) {
  int r;
  g = r;
  h = r + 1;

  if (g < h) {
    f(g, h);
  }
  return 0;
}
