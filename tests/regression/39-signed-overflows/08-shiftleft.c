// PARAM: --enable ana.int.interval
#include <goblint.h>
int main()
{
    int r;
    int zero_or_one = 0;
    int top;
    char c;
    r = c << 1U; //NOWARN

    r = c << 128U; //WARN
    r = r << 1U; //WARN
    r = 8 << -2; //WARN

    if(top) { zero_or_one = 1; }

    r = 8 << zero_or_one;

    __goblint_check(r >= 8);
    __goblint_check(r <= 16);

    int regval;
    unsigned long bla = (unsigned long )((1 << ((int )regval >> 6)) << 20);  //WARN

    return 0;
}
