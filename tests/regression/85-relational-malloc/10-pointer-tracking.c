// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none --enable ana.apron.pointer_tracking --disable warn.integer
#include <stdio.h>
#include <stdlib.h>

int main()
{
    int top;
    int len;

    if (top)
        len = 10;
    else
        len = 5;

    int *p = malloc(len * sizeof(int));
    int *q = p;
    int x = 0;

    while (x++ < len)
    {
        int tmp = *q; // NOWARN
        q++;
        int tmp = *q; // WARN
    }

    return 0;
}
