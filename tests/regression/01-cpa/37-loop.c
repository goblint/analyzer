// PARAM: --sets solver td3 --enable ana.int.interval --enable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
#include <assert.h>
int main() {
  int i = 0;
  for(; i < 4; i++) {
  }
  assert(i == 4);
  return 0;
}
