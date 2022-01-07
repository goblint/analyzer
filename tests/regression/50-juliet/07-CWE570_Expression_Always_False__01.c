#include <stdio.h>
#include <stdbool.h>

bool gFalse = false;

void main()
{
    if (gFalse) // WARN: expression is always false
    {
        printf("Never prints");
    }
}
