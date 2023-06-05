// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval
#include <stdio.h>

int main()
{
    int num = 1;

loop: // NOTERM
    printf("Current number: %d\n", num);
    num++;

    goto loop;

    return 0;
}
