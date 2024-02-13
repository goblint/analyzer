// SKIP PARAM: --enable ana.arrayoob --enable ana.int.interval   --set ana.activated[+] apron --disable warn.integer
//  This file contains known limitations of the current relational array out-of-bounds analysis.
int f(char ptr[], unsigned len)
{
    for (unsigned int i = 0; i < len; i++)
    {
        // due to the pointer arithmetic, the index expressions for these are `(ptr + i) - ptr` and `(ptr + i - 1U) - ptr` respectively
        // We therefore fail to evalulate if this is smaller than our ghost variable
        ptr[i] = 42;     // WARN
        ptr[i - 1] = 42; // TODO NOWARN
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
    f(ptr + 1, len);

    char *ptr2 = ptr + 2;
    for (unsigned int i = 0; i < len; i++)
    {
        char s = ptr2[i - 1]; // TODO NOWARN the index expression for this is "any_index"
    }
}
