#include <stdlib.h>

int main() {
  int (*fp)() = &rand;
  abort();
  fp(); // NOWARN (No suitable function to call)
  return 0;
}
