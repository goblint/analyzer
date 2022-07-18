// PARAM: --enable ana.int.interval
#include<stdio.h>
#include <stdlib.h>

void main()
{
    char data;
    for (int i = 0; i < 100; i++)
    {
        // value of data is pseudo-random
        data = rand();
        if(data < 0) // avoid potential overflow
        {
            char result = data * 2; // WARN: potential underflow
            printf("%hhd\n", result);
        }
    }
}
