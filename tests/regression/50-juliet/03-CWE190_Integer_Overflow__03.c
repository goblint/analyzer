// PARAM: --enable ana.int.interval
#include<stdio.h>

static int badStatic = 0;

static void badSink(unsigned int data)
{
    if (badStatic)
    {
        {
            int result = data * data; // WARN: potential overflow
            printf("%hhd\n", result);
        }
    }
}

void main()
{
    int data;
    fscanf (stdin, "%d", &data);
    badStatic = 1; // if-condition will always be true
    badSink(data);
}
