// PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.int.interval
#include <stdio.h>
#include <goblint.h>

int main(void)
{
    int top;
    int x;
    if (top)
        x = 5;
    else
        x = 10;

    for (int i = 0; i < x * sizeof(char); i++)
    {
        __goblint_check(i <= x);
    }

    for (int i = 0; i < x * sizeof(int); i++)
    {
        __goblint_check(i <= x * sizeof(int));
    }

    for (int i = 0; i < x * sizeof(long) / sizeof(char); i++)
    {
        __goblint_check(i <= x * sizeof(long) / sizeof(char));
    }

    return 0;
}