#include <assert.h>

static int glob = 5;
extern void mywrite(int *x);

int main() {
  int i=0;
  int j=5;
  mywrite(&i);
  assert(i == 0); // UNKNOWN!
  assert(glob == 5);
  assert(j == 5);
  return 0;
}

