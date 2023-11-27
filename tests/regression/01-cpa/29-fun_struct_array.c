#include <goblint.h>

void QQ(){
}

struct a{
  int aa;
  int qq;
};

int main(){
  int i = 1;

  struct a A[1] = {50, (unsigned long)&QQ};

  __goblint_check(A[0].aa == 50);
  __goblint_check(A[0].qq == (unsigned long)&QQ); // UNKNOWN

  return 0;
}
