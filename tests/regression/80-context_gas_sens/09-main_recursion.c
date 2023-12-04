// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Checks if only recursion in main works properly + boundary check
// TODO
#include <stdio.h>

int num_iterat = 11; // WHY is context gas value + 1 the limit???

/*
main mit 11: 10 -> 9
main mit 10:  9 -> 8
main mit  9:  8 -> 7
main mit  8:  7 -> 6
main mit  7:  6 -> 5
main mit  6:  5 -> 4
main mit  5:  4 -> 3
main mit  4:  3 -> 2
main mit  3:  2 -> 1
main mit  2:  1 -> 0
main mit  1:  0 -> 0
main mit  0:  0 -> 0
*/

int main(void)
{
    if (num_iterat > 0)
    {
        num_iterat--;
        int res = main();
        __goblint_check(res == 5);
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
