// PARAM: --enable ana.int.interval
#include<stdio.h>
#include <stdlib.h>

void main()
{
    char data;
    char multiply;

    fscanf(stdin, "%c", &data);
    multiply = (char)(rand() * 3);

    if(data > 0 && multiply > 0) // avoid potential underflow
    {
        char result = data * multiply;  // WARN: potential overflow
        printf("%hhd\n", result);
    }
}
