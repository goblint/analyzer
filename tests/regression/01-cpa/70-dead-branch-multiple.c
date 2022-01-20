#include <assert.h>

int main() {
  int a = 1;
  int b = 0;

  if (a && b) { // WARN
    assert(0); // NOWARN (unreachable)
  }

  return 0;
}
