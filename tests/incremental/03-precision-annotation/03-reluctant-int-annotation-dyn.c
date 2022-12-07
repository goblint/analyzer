#include <goblint.h>

typedef int int_to_int_fun (int);

int f(int in){
  while(in < 17) {
    in++;
  }
  __goblint_check(in == 17); //UNKNOWN
  return in;
}

int_to_int_fun *get_fun(){
  return &f;
}

int main() {
  int_to_int_fun *fun = get_fun();
  int a = 0;
  __goblint_check(a); // FAIL!
  a = fun(a);
  __goblint_check(a == 17); //UNKNOWN
  return 0;
}
