// PARAM: --enable ana.arrayoob  --enable ana.int.interval --set ana.activated[+] apron

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    srand(time(NULL));
    int len = (rand() % 32) + 2;
    int len2 = (rand() % 32) + 2;
    int len3 = (rand() % 32) + 2;
    int arr[len][len2][len3];

    // three dimensions doesn't work with polyhedra probably due to widening
    // we are missing the `i<len` relation with polyhedra
    for (int i = 0; i < len; i++)
    {
        for (int j = 0; j < len2; j++)
        {
            for (int k = 0; k < len3; k++)
            {
                arr[i][j][k] = 3;     // NOWARN
                arr[i + j][j][k] = 3; // WARN
                arr[i][j + i][k] = 3; // WARN
                arr[i][j][k - k] = 3; // NOWARN
                arr[i - 1][j][k] = 3; // WARN
                arr[i][j - 1][k] = 3; // WARN
                arr[i][j][k - 1] = 3; // WARN
                arr[i + 1][j][k] = 3; // WARN
                arr[i][j + 1][k] = 3; // WARN
                arr[i][j][k + 1] = 3; // WARN

                int f = arr[i][j][k]; // NOWARN
                f = arr[i + j][j][k]; // WARN
                f = arr[i][j + i][k]; // WARN
                f = arr[i][j][k - k]; // NOWARN
                f = arr[i + 1][j][k]; // WARN
                f = arr[i][j + 1][k]; // WARN
                f = arr[i][j][k + 1]; // WARN
                f = arr[i - 1][j][k]; // WARN
                f = arr[i][j - 1][k]; // WARN
                f = arr[i][j][k - 1]; // WARN
            }
        }
    }

    return 0;
}
