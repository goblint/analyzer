//PARAM: --enable ana.int.def_exc
#include<assert.h>

int main() {
  unsigned int top;
  unsigned int x;

  if (top == 3){
    return 0;
  }

  x = top * 1073741824u;
  assert(x != 3221225472u); // UNKNOWN!
  return 0;
}
