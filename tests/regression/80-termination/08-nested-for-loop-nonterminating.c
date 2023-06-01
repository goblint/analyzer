// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int outerCount, innerCount;

    for (outerCount = 1; outerCount <= 3; outerCount++) // NOTERM
    {
        for (innerCount = 1;; innerCount++) // NOTERM
        {
            printf("(%d, %d) ", outerCount, innerCount);
        }

        printf("\n");
    }

    return 0;
}
