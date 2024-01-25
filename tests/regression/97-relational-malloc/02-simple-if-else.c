// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval  --set ana.activated[+] apron  --set ana.apron.domain polyhedra
int main()
{
    int len = rand();
    int top;

    if (top)
    {
        len = 5;
    }
    else
    {
        len = 10;
    }

    char *ptr = malloc(len);
    for (int i = 0; i < len; i++)
    {
        int t = rand();
        if (t > len)
        {
            t = len - 1;
        }
        else
        {
            t = t - 1;
            if (t < 0)
            {
                t = 0;
            }
        }
        char s = ptr[i]; // NOWARN
    }
}