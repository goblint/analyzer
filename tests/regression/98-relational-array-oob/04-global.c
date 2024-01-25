// PARAM: --enable ana.arrayoob --enable ana.int.interval   --set ana.activated[+] apron   --set sem.int.signed_overflow assume_none

int readUntil(char arr[], unsigned len)
{
    for (unsigned int i = 0; i < len; i++)
    {
        char s = arr[i];     // NOWARN
        char s = arr[i - 1]; // WARN
        char s = arr[i + 1]; // WARN
        char s = arr[i - i]; // NOWARN
        char s = arr[i + i]; // WARN

        arr[i] = 42;      // NOWARN
        arr[i - 1] = 42;  // WARN
        arr[i + 1] = -42; // WARN
        arr[i - i] = -42; // NOWARN
        arr[i + i] = -42; // WARN
    }
}

int main()
{
    unsigned int len;
    unsigned int top;

    if (top)
        len = 5;
    else
        len = 10;

    char ptr[len];
    readUntil(ptr, len);
}
