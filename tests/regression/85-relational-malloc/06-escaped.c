// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron  --set ana.apron.domain polyhedra
int len;

char *createPointer(char *f)
{
    int top;
    if (top)
        len = 5;
    else
        len = 10;
    char *ptr;
    ptr = malloc(sizeof(char) * len);

    for (int i = 0; i < len; i++)
    {
        char s = ptr[i]; // NOWARN
    }
    return ptr;
}

int main()
{

    char *p = createPointer(p);
    for (int i = 0; i < len; i++)
    {
        p[i] = 0; //NOWARN
    }
}