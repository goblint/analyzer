// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra
char *readUntil(char *f)
{
    int len;
    int top;
    int len2l = 5;
    if (top)
        len = 5;
    else
        len = 10;
    char *ptr;
    int rand;
    ptr = malloc(sizeof(char) * len);

    for (int i = 0; i < len; i++)
    {
        char s = ptr[i]; //NOWARN
    }
    return ptr;
}

int main()
{
    int top;
    int len;
    if (top)
        len = 5;
    else
        len = 10;
    char *ff = malloc(sizeof(char) * len);
    for (int i = 0; i < len; i++)
    {
        ff[i] = 0;
    }
}