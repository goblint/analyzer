// PARAM: --enable ana.arrayoob --set ana.activated[+] apron --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int length[2];

int main()
{
    srand(time(NULL));
    int len = (rand() % 32) + 3;
    char arr[len];

    for (int i = 0; i < len; i++)
    {
        arr[i] = 32;     // NOWARN
        arr[i + 1] = 32; // WARN
        arr[i - 1] = 32; // WARN
        arr[i + i] = 32; // WARN

        int tmp = arr[i]; // NOWARN
        tmp = arr[i + 1]; // WARN
        tmp = arr[i - 1]; // WARN
        tmp = arr[i + i]; // WARN
    }

    scanf("%d", &(length[0]));
    if (length[0] < 0)
    {
        length[0] = 0;
    }
    long arr2[length[0]];
    for (int i = 0; i < length[0]; i++)
    {
        arr2[i] = 32;     //TODO NOWARN add unrolled variables to apron domain?
    }

    return 0;
}
