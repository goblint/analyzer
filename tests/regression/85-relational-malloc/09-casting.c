// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none --disable warn.integer
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{

    unsigned int len = rand();

    int *p = malloc(sizeof(int) * len);

    char *t = p;
    for (int i = 0; i < sizeof(int) * len; i++)
    {
        *(t + i) = 2;        // NOWARN
        *(t + 1) = 2;        // NOWARN
        *(t + 2) = 2;        // NOWARN
        *(t + 4) = 2;        // WARN
        *(t - 1) = 2;        // WARN
        char tmp = *(t + i); // NOWARN
    }

    free(p);

    long *p = malloc(sizeof(long) * len);

    int *t = p;
    for (int i = 0; i < len; i++)
    {
        *(t + i) = 2;       // NOWARN
        *(t + 1) = 2;       // NOWARN
        *(t + 2) = 2;       // WARN
        *(t + 4) = 2;       // WARN
        *(t - 1) = 2;       // WARN
        int tmp = *(t + i); // NOWARN
    }

    for (int i = 0; i < sizeof(long) / sizeof(int) * len; i++)
    {
        *(t + i) = 2;       // NOWARN
        *(t + 1) = 2;       // NOWARN
        *(t + 2) = 2;       // WARN
        *(t + 4) = 2;       // WARN
        *(t - 1) = 2;       // WARN
        int tmp = *(t + i); // NOWARN
    }

    free(p);
}
