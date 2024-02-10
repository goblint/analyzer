// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.apron.pointer_tracking  --set sem.int.signed_overflow assume_none --disable warn.integer
#include <stdlib.h>
int readUntil(char *arr, int len)
{
    for (int i = 0; i < len; i++)
    {
        char s = *arr;  // WARN
        s = *(arr - 1); // NOWARN
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
    readUntil(ptr + 1, len);

    // check pointer relation remains after function call
    for (int i = 0; i < len; i++)
    {
        char s = *ptr;  // NOWARN
        s = *(ptr - 1); // WARN
        ptr = ptr + 1;
    }
}