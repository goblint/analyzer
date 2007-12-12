#include <assert.h>

extern void write(int *x);

int main() {
  int i=0;
  write(&i);
  assert_unknown(i);
  return 0;
}

