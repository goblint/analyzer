#include<assert.h>

int main() {
  int c = 256;
  char* a = (char*)&c;
  *(a) = 'A';
  assert(c==65); // FAIL
  return 0;
}
