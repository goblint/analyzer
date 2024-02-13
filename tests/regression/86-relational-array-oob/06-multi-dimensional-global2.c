// SKIP PARAM: --enable ana.arrayoob  --enable ana.int.interval --set ana.activated[+] apron --set ana.activated[+] taintPartialContexts

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int len1;
int len2;
int len3;
int len4;

int f(long ***arr[])
{
    long ***t;
    for (int i = 0; i < len1; i++)
        t = arr[i]; // TODO NOWARN for some reason eval_offset doesn't know the index epxression here
}

int main()
{
    scanf("%d", &len1);
    scanf("%d", &len2);
    scanf("%d", &len3);
    scanf("%d", &len4);

    if (len1 < 0 || len2 < 0 || len3 < 0 || len4 < 0)
        return 1;

    long arr[len1][len2][len3][len4];

    for (int i = 0; i < len1; i++)
        for (int j = 0; j < len2; j++)
            for (int k = 0; k < len3; k++)
                for (int l = 0; l < len4; l++)
                {
                    arr[i][j][k][l] = 3;      // NOWARN
                    long f = arr[i][j][k][l]; // NOWARN
                }
    f(&arr);

    long ***f;
    for (int i = 0; i < len1; i++)
        f = arr[i]; // NOWARN

    return 0;
}
