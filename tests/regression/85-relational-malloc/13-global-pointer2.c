// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] taintPartialContexts  --set ana.activated[+] apron  --set ana.apron.domain polyhedra  --set ana.activated[+] allocVarEscaped --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

int len;
int *gptr;

void *t_other()
{
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;     // NOWARN
        gptr[i + 1] = 42; // WARN
        gptr[i - 1] = 42; // WARN

        int tmp = gptr[i];     // NOWARN
        int tmp = gptr[i + 1]; // WARN
        int tmp = gptr[i - 1]; // WARN
    }
}

int main()
{
    len = rand();
    len %= 10;
    gptr = malloc(sizeof(int) * len);

    t_other();
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;     // NOWARN
        gptr[i + 1] = 42; // WARN
        gptr[i - 1] = 42; // WARN

        int tmp = gptr[i];     // NOWARN
        int tmp = gptr[i + 1]; // WARN
        int tmp = gptr[i - 1]; // WARN
    }
    free(gptr);
    return 0;
}