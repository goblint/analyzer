#include<assert.h>

int main() {
  int i;
  unsigned long long j;

  i = 10;
  j = 100;

  j = (unsigned long long) i;
  assert(j == 10);
}
