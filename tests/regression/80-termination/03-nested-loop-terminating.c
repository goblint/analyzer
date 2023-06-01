// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    int rows = 3;
    int columns = 4;
    int i = 1;

    // Outer while loop for rows
    while (i <= rows)
    { // TERM
        int j = 1;

        // Inner while loop for columns
        while (j <= columns)
        { // TERM
            printf("(%d, %d) ", i, j);
            j++;
        }

        printf("\n");
        i++;
    }

    return 0;
}
