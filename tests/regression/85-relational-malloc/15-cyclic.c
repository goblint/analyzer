// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] apron  --set ana.apron.domain polyhedra  --enable ana.int.interval --set ana.activated[+] allocVarEscaped --enable ana.apron.pointer_tracking
#include <stdio.h>

int len;
int *gptr;

void foo1(int c)
{

    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;     // NOWARN
        gptr[i + 1] = 42; // WARN
        gptr[i - 1] = 42; // WARN

        int tmp = gptr[i];      // NOWARN
        int tmp2 = gptr[i + 1]; // WARN
        int tmp3 = gptr[i - 1]; // WARN
    }
    if (c < 10)
        foo2(c + 1);
    // check ghost variabels remain after function call
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;     // NOWARN
        gptr[i + 1] = 42; // WARN
        gptr[i - 1] = 42; // WARN

        int tmp = gptr[i];      // NOWARN
        int tmp2 = gptr[i + 1]; // WARN
        int tmp3 = gptr[i - 1]; // WARN
    }
}

void foo2(int c)
{

    if (c < 10)
        foo1(c + 1);
}

int main()
{
    int myInt;
    scanf("%d", &myInt);

    if (myInt <= 0)
    {
        myInt = 1;
    }
    len = myInt;

    gptr = malloc(sizeof(int) * len);

    foo1(0);
    return 0;
}
