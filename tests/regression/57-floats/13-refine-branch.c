// PARAM: --enable ana.float.interval
#include <goblint.h>
int main()
{
  double z;
  int x;

  if(z) {
    // z may NOT be refined to range only in the values of int here(!)
    __goblint_check(__builtin_isfinite(z)); //UNKNOWN!
  } else {

  }

}
