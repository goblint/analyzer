// PARAM: --enable ana.float.interval --set ana.activated[+] tmpSpecial

#include <math.h>

int main() /* check_lower_bound */
{
    float x;

    if(!(!__builtin_isnan (x) && !__builtin_isinf_sign (x))) abort();
    float y = cosf(x);
    if(!(__builtin_isgreaterequal(y, -1.0f)))
      __goblint_check(0); // NOWARN (unreachable)
    return 0;

}
