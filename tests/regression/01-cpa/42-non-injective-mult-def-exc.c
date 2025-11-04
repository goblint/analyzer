//PARAM: --enable ana.int.def_exc
#include <goblint.h>

int main() {
  unsigned int top;
  unsigned int x;

  if (top == 3){
    return 0;
  }

  x = top * 1073741824u;
  __goblint_check(x != 3221225472u); // UNKNOWN!
  return 0;
}
