// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval
#include <stdio.h>

int main()
{
    int num = 1;

loop: // TERM
    printf("Current number: %d\n", num);
    num++;

    if (num <= 10)
    {
        goto loop;
    }

    return 0;
}
