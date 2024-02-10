// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.apron.pointer_tracking  --set sem.int.signed_overflow assume_none --disable warn.integer
#include <stdlib.h>
int f(char *arr, int len)
{
    char *arr2 = arr;
    char **arrPtr = &arr;
    *arrPtr = *arrPtr + 1;
    for (int i = 0; i < len; i++)
    {
        char s = *arr;  // WARN
        s = *(arr - 1); // WARN

        char s = *arr2;  // WARN
        s = *(arr2 - 1); // NOWARN

        arr2 = arr2 + 1;
        arr = arr + 1;
    }
}

int main()
{
    int len;
    int top;
    if (rand())
        len = 5;
    else
        len = 10;
    char *ptr = malloc(sizeof(char) * len);
    f(ptr + 1, len);

    // check pointer relation remains after function call
    char *ptr2 = ptr;
    for (int i = 0; i < len; i++)
    {
        char s = *ptr;  // NOWARN
        s = *(ptr - 1); // WARN

        char s = *ptr2;  // NOWARN
        s = *(ptr2 - 1); // WARN

        ptr = ptr + 1;
        ptr2 = ptr2 + 1;
    }
}