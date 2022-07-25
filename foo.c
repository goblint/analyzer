#include <assert.h>

int main() {
  int i = 0;
  while (i < 100) {
    i++;
  }
  assert(i == 100);
  return 0;
}