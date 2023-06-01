// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int i = 1;

    do // TERM
    {
        printf("Inside the do-while loop\n");
        i++;
    } while (i <= 5);

    printf("Exited the loop\n");
    return 0;
}
