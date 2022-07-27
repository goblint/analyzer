// PARAM: --enable ana.float.interval
#include <assert.h>
int main()
{
  double z;
  int x;

  if(z) {
    // z may NOT be refined to range only in the values of int here(!)
    assert(__builtin_isfinite(z)); //UNKNOWN!
  } else {

  }

}
