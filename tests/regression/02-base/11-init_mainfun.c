// PARAM: --set otherfun "['f']" --enable exp.earlyglobs
#include <assert.h>

int glob;

void f() {
  int i = glob;
  assert(i == 0);
}

int main(void *arg) {
  return 0;
}
