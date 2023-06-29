// TODO TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdlib.h>
#include <stdio.h>

int main()
{
    int short_run, i = 0;

    while (i < 90 && short_run != 1)
    {
        i++;
        if (rand())
        {
            short_run = 1;
        }
    }
}