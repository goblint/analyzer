// PARAM: --enable ana.arrayoob --enable ana.int.interval   --set ana.activated[+] apron --disable warn.integer --set ana.apron.domain polyhedra 

int f(char ptr[], unsigned len)
{
    for (unsigned int i = 0; i < len; i++)
    {
        char s = ptr[0]; // NOWARN
        char s = ptr[i]; // WARN

        ptr[i] = 42;     // NOWARN
        ptr[i - 1] = 42; // WARN
    }
}

int f2(char ptr[], unsigned len)
{
    for (unsigned int i = 0; i < len; i++)
    {
        char s = ptr[0]; // NOWARN
        char s = ptr[i]; // TODO NOWARN for some reason eval_offset has as an index "any_index", which causes our analysis to fail
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
    char *ptr2 = ptr + 7;
    f(ptr, len);

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
        ptr[i + i] = -42; // WARN
    }

    f2(ptr, len);

    for (unsigned int i = 0; i < len; i++)
    {
        char s = ptr[0]; // NOWARN
        char s = ptr[i]; // NOWARN
    }
}
