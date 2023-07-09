// SKIP TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
// Goblint does not finish this test
#include <stdio.h>

int main()
{
    int i, j, k;

    // Outer loop
    for (i = 1; i <= 5; i++)
    {
        // Inner loop 1
        for (j = 1; j <= i; j++)
        {
            printf("%d ", j);
        }
        printf("\n");

        // Inner loop 2
        for (k = i; k >= 1; k--)
        {
            printf("%d ", k);
        }
        printf("\n");
    }

    // Additional loop
    for (i = 5; i >= 1; i--)
    {
        for (j = i; j >= 1; j--)
        {
            printf("%d ", j);
        }
        printf("\n");
    }

    // Loop with conditions
    for (i = 1; i <= 10; i++)
    {
        if (i % 2 == 0)
        {
            printf("%d is even\n", i);
        }
        else
        {
            printf("%d is odd\n", i);
        }
    }

    // Loop with nested conditions
    for (i = 1; i <= 10; i++)
    {
        printf("Number: %d - ", i);
        if (i < 5)
        {
            printf("Less than 5\n");
        }
        else if (i > 5)
        {
            printf("Greater than 5\n");
        }
        else
        {
            printf("Equal to 5\n");
        }
    }

    // Loop with a break statement
    for (i = 1; i <= 10; i++)
    {
        printf("%d ", i);
        if (i == 5)
        {
            break;
        }
    }
    printf("\n");

    // Loop with a continue statement
    for (i = 1; i <= 10; i++)
    {
        if (i % 2 == 0)
        {
            continue;
        }
        printf("%d ", i);
    }
    printf("\n");

    // Loop with complex conditions
    for (i = 1; i <= 10; i++)
    {
        if (i > 5 && i % 2 == 0)
        {
            printf("%d ", i);
        }
    }
    printf("\n");

    // Loop with multiple variables
    int a, b, c;
    for (a = 1, b = 2, c = 3; a <= 10; a++, b += 2, c += 3)
    {
        printf("%d %d %d\n", a, b, c);
    }

    return 0;
}
