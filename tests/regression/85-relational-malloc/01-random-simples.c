// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra

#include <stdlib.h>

char *arr;
int main()
{
    unsigned int len = rand();

    arr = malloc(sizeof(char) * len);

    for (int i = 0; (unsigned int)i < len; i++)
    {
        arr[i] = 42;      // NOWARN
        arr[i + 1] = 127; // WARN
        arr[i - 1] = 42;  // WARN
        int t = arr[i];   // NOWARN
        t = arr[i + 1];   // WARN
        t = arr[i - 1];   // WARN

        int j = i * 2;
        // a limitation of the current implementation as we compute compute  offset(op1) + op2 < arr$len
        t = arr[ j - i ]; // UNKNOWN 
    }
}
