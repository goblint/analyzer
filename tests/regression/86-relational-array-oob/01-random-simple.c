// PARAM: --enable ana.arrayoob    --set ana.activated[+] apron   --set ana.apron.domain octagon 


#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    srand(time(NULL));
     int len = (rand() % 32) + 3;
    char arr[len];

    for (int i = 0; i < len; i++)
    {
        arr[i] = 32;     // NOWARN
        arr[i + 1] = 32; // WARN
        arr[i - 1] = 32; // WARN
        arr[i + i] = 32; // WARN

        int tmp = arr[i];     // NOWARN
        int tmp = arr[i + 1]; // WARN
        int tmp = arr[i - 1]; // WARN
        int tmp = arr[i + i]; // WARN
    }

    return 0;
}
