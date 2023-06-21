// LOCAL_TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int num = 1;

loop:
    printf("Current number: %d\n", num);
    num++;

    if (num <= 10)
    {
        goto loop;
    }

    return 0;
}
