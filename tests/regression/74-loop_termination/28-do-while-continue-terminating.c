// TODO TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int i = 1;

    do
    {
        i++;
        printf("Inside the do-while loop\n");
        if (i % 2 == 0) {

        printf("Skipping %i is even\n", i);
            continue; // This is handled as an goto to line 8 and there an up-jumping goto
        }
    } while (i <= 5);

    printf("Exited the loop\n");
    return 0;
}
