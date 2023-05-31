#include <stdio.h>
#include <stdlib.h>

void main()
{
    // (0 <= uInt < UINT_MAX), uInt is pseudo-random
    unsigned int uInt = (unsigned int)(rand());

    if (uInt >= 0) // WARN: expression is always true
    {
        printf("Always prints\n");
    }
}
