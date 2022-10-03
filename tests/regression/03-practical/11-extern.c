#include <assert.h>

static int glob = 5;
extern void mywrite(int *x);

int main() {
  int i=0;
  int j=5;
  mywrite(&i);
  __goblint_check(i == 0); // UNKNOWN!
  __goblint_check(glob == 5);
  __goblint_check(j == 5);
  return 0;
}

