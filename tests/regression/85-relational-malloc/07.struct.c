// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval  --set ana.activated[+] apron  --set ana.apron.domain polyhedra --enable ana.apron.pointer_tracking

struct T
{
    int a;
    int b;
};

int main()
{
    int len;
    int top;

    if (top)
        len = 5;
    else
        len = 10;

    struct T *ptr = malloc(sizeof(struct T) * len - 1);
    for (int i = 0; i < len; i++)
    {
        int s = ptr[i].a; // NOWARN
        s = ptr[i].b; // WARN
    }
}