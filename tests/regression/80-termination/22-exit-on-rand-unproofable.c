// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdlib.h>

int main()
{
    int forever = 1;

    while (forever == 1)
    {
        if (rand()) //May exit, may not
        {
            forever = 0;
        }
    }
}