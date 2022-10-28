// PARAM: --enable ana.float.interval
// Taken from CBMC's regression test suite
// (http://svn.cprover.org/svn/cbmc/trunk/regression/cbmc/).
#include <math.h>
#include <goblint.h>
#include <float.h>

int main()
{
  double d;

  // first check constants

  __goblint_check((isnormal(FLT_MAX)));
  __goblint_check((isinf(HUGE_VAL)));
  __goblint_check((isinf(HUGE_VALF)));
//  __goblint_check((isinf(HUGE_VALL)));
  __goblint_check((isinf(INFINITY)));
  __goblint_check((isnan(NAN)));

  // check +
  __goblint_check((isinf(INFINITY+INFINITY)));
  __goblint_check((isnan(-INFINITY+INFINITY)));
  __goblint_check((INFINITY+INFINITY>0));
  __goblint_check((isnan(NAN+d)));
  __goblint_check((isnan(NAN+INFINITY)));

  // check -
  __goblint_check((isnan(INFINITY-INFINITY)));
  __goblint_check((isinf(-INFINITY-INFINITY)));
  __goblint_check((-INFINITY-INFINITY<0));
  __goblint_check((isnan(NAN-d)));
  __goblint_check((isnan(NAN-INFINITY)));

  // check *
  __goblint_check((isinf(INFINITY*INFINITY))); //TODO
  __goblint_check((isinf(-INFINITY*INFINITY))); //TODO
  __goblint_check((INFINITY*INFINITY>0)); //TODO
  __goblint_check((-INFINITY*INFINITY<0)); //TODO
  __goblint_check((isnan(NAN*d))); //TODO
  __goblint_check((isnan(NAN*INFINITY))); //TODO
  __goblint_check((isnan(INFINITY*0))); //TODO
  __goblint_check((signbit(1.0*-0.0))); //TODO
  __goblint_check((!signbit(1.0*0.0))); //TODO

  // check /
  __goblint_check((isnan(INFINITY/INFINITY))); //TODO
  __goblint_check((isnan(-INFINITY/INFINITY))); //TODO
  // work around compiler warning
  int zero=0;
  __goblint_check((isinf(INFINITY/zero))); //TODO
  __goblint_check((0.0/INFINITY==0)); //TODO
  __goblint_check((1.0/INFINITY==0)); //TODO
  __goblint_check((signbit(-1.0/INFINITY))); //TODO
  __goblint_check((signbit(1.0/-INFINITY))); //TODO
  __goblint_check((INFINITY/-2<0)); //TODO
  __goblint_check((isinf(1.0/0.0))); //TODO
  __goblint_check((isinf(INFINITY/2))); //TODO
  __goblint_check((isnan(0.0/0.0))); //TODO
  __goblint_check((isnan(NAN/d))); //TODO
  __goblint_check((isnan(NAN/INFINITY))); //TODO
  __goblint_check((signbit(-0.0/1))); //TODO
}
