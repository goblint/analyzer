// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{

    unsigned int len = rand();

    int *arr;
    arr = malloc(sizeof(int) * len);

    for (int i = 0; i < len; i++)
    {
        char *t;
        if (rand())
        {
            t = arr - 1;
        }
        else
        {
            t = arr + 1;
        }
        *(t + 1) = 2; // NOWARN
        *(t + i) = 2; // WARN
        *(t+len) =2;
    }
}
