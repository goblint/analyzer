// PARAM: --enable ana.float.interval
// Taken from CBMC's regression test suite
// (http://svn.cprover.org/svn/cbmc/trunk/regression/cbmc/).
#include <math.h>
#include <goblint.h>

int main()
{
  // constants
  __goblint_check(!(1.0!=2.0)); //FAIL
  __goblint_check(!(1.0==1.0)); //FAIL
  __goblint_check(!(1.0<2.0)); //FAIL
  __goblint_check(!(!(-1.0<-2.0))); //FAIL
  __goblint_check(!(2.0>1.0)); //FAIL
  __goblint_check(!(!(-2.0>-1.0))); //FAIL
  __goblint_check(!(!(2.0<2.0))); //FAIL
  __goblint_check(!(!(-2.0<-2.0))); //FAIL
  __goblint_check(!(!(2.0>2.0))); //FAIL
  __goblint_check(!(!(-2.0>-2.0))); //FAIL
  __goblint_check(!(2.0<=2.0)); //FAIL
  __goblint_check(!(-2.0<=-2.0)); //FAIL
  __goblint_check(!(2.0>=2.0)); //FAIL
  __goblint_check(!(-2.0>=-2.0)); //FAIL
  __goblint_check(!(1.0<=2.0)); //FAIL
  __goblint_check(!(!(-1.0<=-2.0))); //FAIL
  __goblint_check(!(2.0>=1.0)); //FAIL
  __goblint_check(!(!(-2.0>=-1.0))); //FAIL

  // variables
  float a, b;
  if (!(a==1 && b==2)) {abort();}

  __goblint_check(!(a!=b)); //FAIL
  __goblint_check(!(a==a)); //FAIL
  __goblint_check(!(a<b)); //FAIL
  __goblint_check(!(!(-a<-b))); //FAIL
  __goblint_check(!(b>a)); //FAIL
  __goblint_check(!(!(-b>-a))); //FAIL
  __goblint_check(!(!(b<b))); //FAIL
  __goblint_check(!(!(-b<-b))); //FAIL
  __goblint_check(!(!(b>b))); //FAIL
  __goblint_check(!(!(-b>-b))); //FAIL
  __goblint_check(!(b<=b)); //FAIL
  __goblint_check(!(-b<=-b)); //FAIL
  __goblint_check(!(b>=b)); //FAIL
  __goblint_check(!(-b>=-b)); //FAIL
  __goblint_check(!(a<=b)); //FAIL
  __goblint_check(!(!(-a<=-b))); //FAIL
  __goblint_check(!(b>=a)); //FAIL
  __goblint_check(!(!(-b>=-a))); //FAIL
}
