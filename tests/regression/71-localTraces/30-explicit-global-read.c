// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
int c = 222;

int f() { return c - 250; }
void main() {
  int x;
  int y = 3;
  if (2 < 7 + c - 111) {
    x = f();
    x = 1 + y;
  } else {
    x = -12;
  }
}
