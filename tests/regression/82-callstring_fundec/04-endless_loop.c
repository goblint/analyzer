// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Will result in an endless loop without context insensitive analysis
#include <stdio.h>

int num_iterat = 2;

// main -> main -> ...
// [main, main, ...]
int main(void)
{
    if (num_iterat > 0)
    {
        num_iterat++;
        int res = main();
        __goblint_check(res == 5); // UNKNOWN
        return res;
    }
    else
    {
        if (num_iterat == 0)
        {
            return 5;
        }
        return 2;
    }
}