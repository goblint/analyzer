#include<stdio.h>

void main()
{
    char data;
    fscanf(stdin, "%c", &data);
    {
        data--; // TODO WARN: potential underflow
        char result = data;
        printf("%hhd\n", result);
    }
}
