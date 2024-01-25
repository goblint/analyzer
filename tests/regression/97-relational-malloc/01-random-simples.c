// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra
#include <stdlib.h>

int main()
{
    int len = rand();

    char *arr;
    arr = malloc(sizeof(char) * len);

    for (int i = 0; i < len; i++)
    {
        char tmp = *(arr);   // NOWARN
        arr[i] = 42;     // NOWARN
        arr[i + 1] = 127; // WARN
        arr[i - 1] = 42; // WARN
    }
}
