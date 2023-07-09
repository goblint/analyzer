// TODO TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include<stdio.h>

int main() {

    // Loop with a continue statement
    for (int r = 1; r <= 10; r++)
    {
        if (r % 3 == 0)
        {
            continue;
        }
        printf("Loop with Continue: %d\n", r);
    }

    // Loop with multiple conditions
    int s = 1;
    while (s <= 10 && s % 2 == 0)
    {
        printf("Loop with Multiple Conditions: %d\n", s);
        s++;
    }

    // Loop with multiple variables
    int t, u;
    for (t = 1, u = 10; t <= 5 && u >= 5; t++, u--)
    {
        printf("Loop with Multiple Variables: %d %d\n", t, u);
    }
    }