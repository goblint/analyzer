#include<assert.h>

int f(int in){
  while(in < 17) {
    in++;
  }
  assert(in == 17); //UNKNOWN
  return in;
}

int main() {
  int a = 0;
  assert(a); // FAIL!
  a = f(a);
  assert(a == 17); //UNKNOWN
  return 0;
}
