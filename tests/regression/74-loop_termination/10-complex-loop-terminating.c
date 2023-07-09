// SKIP TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
// Goblint does not finish this test
#include <stdio.h>

int main()
{
    int i = 1;
    int j = 1;
    int k = 5;

    // Outer while loop
    while (i <= 5)
    {
        // Inner while loop 1
        while (j <= i)
        {
            printf("%d ", j);
            j++;
        }
        printf("\n");
        j = 1;

        // Inner while loop 2
        while (k >= 1)
        {
            printf("%d ", k);
            k--;
        }
        printf("\n");
        k = 5;

        i++;
    }

    // Additional while loop
    i = 5;
    while (i >= 1)
    {
        j = i;
        while (j >= 1)
        {
            printf("%d ", j);
            j--;
        }
        printf("\n");
        i--;
    }

    // Loop with conditions
    i = 1;
    while (i <= 10)
    {
        if (i % 2 == 0)
        {
            printf("%d is even\n", i);
        }
        else
        {
            printf("%d is odd\n", i);
        }
        i++;
    }

    // Loop with nested conditions
    i = 1;
    while (i <= 10)
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
        i++;
    }

    // Loop with a break statement
    i = 1;
    while (i <= 10)
    {
        printf("%d ", i);
        if (i == 5)
        {
            break;
        }
        i++;
    }
    printf("\n");

    // Loop with a continue statement
    i = 1;
    while (i <= 10)
    {
        if (i % 2 == 0)
        {
            i++;
            continue;
        }
        printf("%d ", i);
        i++;
    }
    printf("\n");

    // Loop with complex conditions
    i = 1;
    while (i <= 10)
    {
        if (i > 5 && i % 2 == 0)
        {
            printf("%d ", i);
        }
        i++;
    }
    printf("\n");

    // Loop with multiple variables
    int a = 1;
    int b = 2;
    int c = 3;
    while (a <= 10)
    {
        printf("%d %d %d\n", a, b, c);
        a++;
        b += 2;
        c += 3;
    }

    return 0;
}
