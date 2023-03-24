// PARAM: --enable ana.float.interval
// Taken from CBMC's regression test suite
// (http://svn.cprover.org/svn/cbmc/trunk/regression/cbmc/).
#include <math.h>
#include <goblint.h>
#include <float.h>

int main()
{
  int res = 0;
  if(0.0 / 5.0 == 0) {
    res=1;
  }
  __goblint_check(res == 1);
}
