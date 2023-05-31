// PARAM: --enable ana.int.interval
#include<stdio.h>
#include <stdlib.h>

void main()
{
    // no char because char has unknown signedness (particularly, unsigned on arm64)
    signed char data;
    for (int i = 0; i < 100; i++)
    {
        // value of data is pseudo-random
        data = rand();
        if(data < 0) // avoid potential overflow
        {
            signed char result = data * 2; // WARN: potential underflow
            printf("%hhd\n", result);
        }
    }
}
