// NON_LOCAL_TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdlib.h>

int main()
{
    int forever, i = 0;

// This loop is not provable, therefore it should throw a warning
    while (i < 4 || forever == 1)
    {
        i++;
        if (i == 4)
        {
            if (rand())
            {
                forever = 1;
            }
        }
    }
}