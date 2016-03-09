// PARAM: --set dbg.debug true
#include <assert.h>

void QQ(){
}

struct a{
  int aa;
  int qq;
};

struct a A[1] = {50, (int)&QQ} ;

int main(){
  int i = 1;

  assert(A[0].aa == 50);
  assert(A[0].qq == &QQ); // UNKNOWN

  return 0;
}
