// NONTERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // Seed the random number generator
    srand(time(NULL));

    if (rand())
    {
        // Loop inside the if part
        for (int i = 1; i >= 0; i++)
        {
            printf("Loop inside if part: %d\n", i);
        }
    }
    else
    {
        // Loop inside the else part
        int j = 1;
        while (j > 0)
        {
            printf("Loop inside else part: %d\n", j);
        }
    }

    return 0;
}
