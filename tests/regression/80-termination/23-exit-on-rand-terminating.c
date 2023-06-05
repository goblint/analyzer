// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval
#include <stdlib.h>

int main()
{
    int shortrun, i = 0;

    while (i < 90 || shortrun == 1)
    {
        i++;
        if (rand())
        {
            shortrun = 1;
        }
    }
}