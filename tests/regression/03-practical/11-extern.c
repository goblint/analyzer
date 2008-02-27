#include <assert.h>

extern void write(int *x);

int main() {
  int i=0;
  write(&i);
  assert(i == 0); // UNKNOWN!
  return 0;
}

