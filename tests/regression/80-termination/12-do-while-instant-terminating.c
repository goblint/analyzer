// LOCAL_TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int i = 0;

    do
    {
        printf("Inside the do-while loop\n");
    } while (i > 0);

    printf("Exited the loop\n");
    return 0;
}
