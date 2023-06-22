// NON_LOCAL_TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int i = 1;

    do
    {
        printf("Inside the do-while loop\n");
        i++;

        if(i%2) {
            printf("Continue as %i is odd\n", i);
            continue;
        }
    } while (i >= 2);

    printf("Exited the loop\n");
    return 0;
}
