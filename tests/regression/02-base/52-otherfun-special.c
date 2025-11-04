// SKIP PARAM: --set otherfun '["magic"]'
#include <pthread.h>
#include <goblint.h>

extern void* magic(void* arg);
int g;

int main() {
  __goblint_check(g == 0); // UNKNOWN! (magic may invalidate)
  return 0;
}
