// PARAM: --enable ana.arrayoob --enable ana.int.interval   --set ana.activated[+] apron   --disable warn.integer

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
    readUntil(ptr+1, len);

    for (unsigned int i = 0; i < len; i++)
    {
        char s = ptr[i];     // NOWARN
        char s = ptr[i - 1]; // WARN
        char s = ptr[i + 1]; // WARN
        char s = ptr[i - i]; // NOWARN
        char s = ptr[i + i]; // WARN

        ptr[i] = 42;      // NOWARN
        ptr[i - 1] = 42;  // WARN
        ptr[i + 1] = -42; // WARN
        ptr[i - i] = -42; // NOWARN
        ptr[i + i] = -42; // WARN
    }
}
