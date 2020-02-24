// PARAM: --set dbg.debug true
#include <assert.h>

void QQ(){
}

struct a{
  int aa;
  int qq;
};

int main(){
  int i = 1;

  struct a A[1] = {50, (unsigned long)&QQ};

  assert(A[0].aa == 50);
  assert(A[0].qq == (unsigned long)&QQ); // UNKNOWN

  return 0;
}
