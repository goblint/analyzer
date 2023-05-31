// PARAM: --enable ana.int.interval
#include<stdio.h>

void main()
{
    char data;
    fscanf(stdin, "%c", &data);
    {
        data--; // WARN: potential underflow
        char result = data;
        printf("%hhd\n", result);
    }
}
