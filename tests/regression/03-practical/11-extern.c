#include <assert.h>

extern void mywrite(int *x);

int main() {
  int i=0;
  mywrite(&i);
  assert(i == 0); // UNKNOWN!
  return 0;
}

