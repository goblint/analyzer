// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false
#include <goblint.h>
int a;

struct h {
  int c;
};

struct i {
  int d;
};

struct h* b;
struct i* c;

const void *d(const int* e) {
  return e + 200;
}

int *f() {
  return (int*) 0;
}

int g(int, struct h *, struct i *) {
  int *j = f();
  d(j);
  __goblint_check(1); // reachable
}

int main() {
  g(a, b, c);
  if (0) {
    __goblint_check(0); // NOWARN (unreachable)
  }
  __goblint_check(1); // reachable
}
