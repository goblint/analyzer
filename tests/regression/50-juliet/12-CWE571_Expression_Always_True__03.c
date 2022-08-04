// PARAM: --enable ana.int.interval
// def_exc doesn't eval_rv/invariant <= INT_MAX using Exc range
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

void main()
{
    int intRand = rand();
    if (intRand <= INT_MAX) // WARN: expression is always true
    {
        printf("Always prints\n");
    }
}
