// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval
#include <stdlib.h>

int main()
{
    int forever, i = 0;

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