// PARAM: --enable ana.arrayoob --enable ana.int.interval   --set ana.activated[+] apron  

int main()
{
    int len;
    int top;

    if (top)
        len = 5;
    else
        len = 10;

    char ptr[len];

    for (int i = 0; i < len; i++)
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
