#include<stdio.h>

void main()
{
    char data;
    fscanf(stdin, "%c", &data);
    if(-data < 0) // avoid potential overflow
    {
        char result = -data * 2; // TODO WARN: potential underflow
        printf("%hhd\n", result);
    }
}
