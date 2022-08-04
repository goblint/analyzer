#include <stdio.h>
#include <stdbool.h>

bool gTrue = true;

void main()
{
    if (gTrue) // WARN: expression is always true
    {
        printf("Always prints\n");
    }
}
