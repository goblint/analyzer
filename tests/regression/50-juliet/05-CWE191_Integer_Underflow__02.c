// PARAM: --enable ana.int.interval
#include<stdio.h>

void main()
{
    char data;
    fscanf(stdin, "%c", &data);
    if(-data < 0) // avoid potential overflow
    {
        char result = -data * 2; // WARN: potential underflow
        printf("%hhd\n", result);
    }
}
