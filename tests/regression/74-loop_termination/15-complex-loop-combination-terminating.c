//  TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main()
{
    // Non-nested loops
    int i;

    // for loop
    for (i = 1; i <= 10; i++)
    {
        printf("For loop iteration: %d\n", i);
    }

    // while loop
    int j = 1;
    while (j <= 10)
    {
        printf("While loop iteration: %d\n", j);
        j++;
    }

    // do-while loop
    int k = 1;
    do
    {
        printf("Do-While loop iteration: %d\n", k);
        k++;
    } while (k <= 10);

    // Nested loops
    int a, b;

    // Nested for and while loop
    for (a = 1; a <= 5; a++)
    {
        int c = 1;
        while (c <= a)
        {
            printf("Nested For-While loop: %d\n", c);
            c++;
        }
    }

    // Nested while and do-while loop
    int x = 1;
    while (x <= 5)
    {
        int y = 1;
        do
        {
            printf("Nested While-Do-While loop: %d\n", y);
            y++;
        } while (y <= x);
        x++;
    }

    // Nested do-while and for loop
    int p = 1;
    do
    {
        for (int q = 1; q <= p; q++)
        {
            printf("Nested Do-While-For loop: %d\n", q);
        }
        p++;
    } while (p <= 5);

    // Additional loops
    int m;

    // Nested while loop with a break statement
    int n = 1;
    while (n <= 5)
    {
        printf("Outer While loop iteration: %d\n", n);
        m = 1;
        while (1)
        {
            printf("Inner While loop iteration: %d\n", m);
            m++;
            if (m == 4)
            {
                break;
            }
        }
        n++;
    }

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

    // Loop with nested conditions
    for (int v = 1; v <= 10; v++)
    {
        printf("Loop with Nested Conditions: %d - ", v);
        if (v < 5)
        {
            printf("Less than 5\n");
        }
        else if (v > 5)
        {
            printf("Greater than 5\n");
        }
        else
        {
            printf("Equal to 5\n");
        }
    }

    // Loop with a label and goto statement
    int w = 1;
start:
    if (w <= 5)
    {
        printf("Loop with Label and Goto: %d\n", w);
        w++;
        goto start;
    }

    return 0;
}
