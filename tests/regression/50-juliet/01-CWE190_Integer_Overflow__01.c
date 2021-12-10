// PARAM: --enable ana.int.interval
#include<stdio.h>

void main()
{
    char data;
    fscanf(stdin, "%c", &data);
    {
        char result = data + 1;  // WARN: potential overflow
        printf("%hhd\n", result);
    }
}
