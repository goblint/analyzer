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


  // On OS X this gets expanded differently than on Linux where it is equivalent to the one below
  // Might make sense to check what is needed for OS X support in the future, but this is not a deal-breaker
  // and not high priority for now.
  // __goblint_check((isnormal(FLT_MAX)));

  __goblint_check((__builtin_isnormal(FLT_MAX)));

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
  __goblint_check((isinf(INFINITY*INFINITY)));
  __goblint_check((isinf(-INFINITY*INFINITY)));
  __goblint_check((INFINITY*INFINITY>0));
  __goblint_check((-INFINITY*INFINITY<0));
  __goblint_check((isnan(NAN*d)));
  __goblint_check((isnan(NAN*INFINITY)));
  __goblint_check((isnan(INFINITY*0)));
  __goblint_check((signbit(1.0*-0.0))); //TODO (we don't distinguish 0 and -0)
  __goblint_check((!signbit(1.0*0.0))); //TODO (we don't distinguish 0 and -0)

  // check /
  __goblint_check((isnan(INFINITY/INFINITY)));
  __goblint_check((isnan(-INFINITY/INFINITY)));
  // work around compiler warning
  int zero=0;
  __goblint_check((isinf(INFINITY/zero))); //TODO (we don't distinguish 0 and -0)
  __goblint_check((0.0/INFINITY==0));
  __goblint_check((1.0/INFINITY==0));
  __goblint_check((signbit(-1.0/INFINITY))); //TODO (we don't distinguish 0 and -0)
  __goblint_check((signbit(1.0/-INFINITY))); //TODO (we don't distinguish 0 and -0)
  __goblint_check((INFINITY/-2<0));
  __goblint_check((isinf(1.0/0.0))); //TODO (we don't distinguish 0 and -0)
  __goblint_check((isinf(INFINITY/2)));
  __goblint_check((isnan(0.0/0.0)));
  __goblint_check((isnan(NAN/d)));
  __goblint_check((isnan(NAN/INFINITY)));
  __goblint_check((signbit(-0.0/1))); //TODO (we don't distinguish 0 and -0)
}
