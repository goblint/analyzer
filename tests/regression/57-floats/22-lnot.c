//PARAM: --enable ana.float.interval
#include<goblint.h>
int main() {
  float x = 0.0f;
  int z = !x; 

  int reach;

  if(z) {
    __goblint_check(1); //Reachable
    reach = 1;
  } else {
    reach = 0;
  }

  __goblint_check(reach == 1);

  float y;
  if (!y) {
    __goblint_check(y == 0.0f);
  } else {
    __goblint_check(1); //Reachable
  }
  
  return 0;
}