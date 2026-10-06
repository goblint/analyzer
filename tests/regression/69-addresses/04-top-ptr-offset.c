#include <stdlib.h>
#include <goblint.h>

struct S {
  int foo;
  int bar;
};

int main() {
  struct S *p; // rand
  int *q;

  q = &p->foo; // TODO NOWARN
  __goblint_check(q == NULL); // UNKNOWN!
  *q = 42; // WARN (may deref NULL)

  q = &p->bar; // TODO NOWARN
  __goblint_check(q != NULL); // TODO
  *q = 42; // TODO NOWARN

  return 0;
}
