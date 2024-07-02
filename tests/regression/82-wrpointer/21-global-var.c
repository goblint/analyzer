// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts

#include <goblint.h>
#include <stdlib.h>

int **i;
int **j;
int counter;

void f() { __goblint_check(*i == *j); }

void recursive_f() {
  __goblint_check(*i == *j);
  counter++;
  if (counter < 25)
    recursive_f();
}

void non_terminating_f() {
  if (*i == *j)
    non_terminating_f();
}

int main(void) {

  j = (int **)malloc(sizeof(int *));
  i = (int **)malloc(sizeof(int *));
  *i = (int *)malloc(sizeof(int));

  *j = *i;
  f();

  recursive_f();

  non_terminating_f();

  __goblint_check(0); // NOWARN (unreachable)

  return 0;
}
