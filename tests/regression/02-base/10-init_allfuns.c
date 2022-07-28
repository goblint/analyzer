// PARAM: --enable allfuns --set ana.activated "['base','threadid','threadflag','escape','mutexEvents','mutex','access','mallocWrapper','assert']"
#include <assert.h>

int glob1 = 5;
int glob2 = 7;

int f() {
  glob1 = 5;
  return 0;
}

int g() {
  __goblint_check(glob1 == 5);
  __goblint_check(glob2 == 7);
  return 0;
}
