#include <stdlib.h>

extern void abort(void);
void reach_error(){}

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: {reach_error();abort();}
  }
  return;
}

int main() {
  int *x = malloc(sizeof(int));
  int **p = malloc(sizeof(int*));

  *x = 5;
  *p = x;

  __VERIFIER_assert(*x == 5);
  __VERIFIER_assert(**p == 5);

  return 0;
}
