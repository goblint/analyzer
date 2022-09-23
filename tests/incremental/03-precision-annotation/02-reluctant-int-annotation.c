#include<assert.h>

int f(int in){
  while(in < 17) {
    in++;
  }
  __goblint_check(in == 17); //UNKNOWN
  return in;
}

int main() {
  int a = 0;
  __goblint_check(a); // FAIL!
  a = f(a);
  __goblint_check(a == 17); //UNKNOWN
  return 0;
}
