// PARAM: --enable ana.int.def_exc --enable ana.int.interval
#include <assert.h>
int main() {
  int i = 0;
  for(; i < 4; i++) {
  }
  assert(i == 4);
  return 0;
}
