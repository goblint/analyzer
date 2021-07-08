// SKIP PARAM: --set otherfun '["magic"]'
#include <pthread.h>
#include <assert.h>

extern void* magic(void* arg);
int g;

int main() {
  assert(g == 0); // UNKNOWN! (magic may invalidate)
  return 0;
}
