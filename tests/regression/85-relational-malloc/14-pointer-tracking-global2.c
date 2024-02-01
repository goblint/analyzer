// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron --set ana.apron.domain polyhedra  --disable warn.integer --enable ana.apron.pointer_tracking
int top;

int readUntil(char *arr, int len)
{
    for (int i = 0; i < len; i++)
    {
        int tmp = 2 * i;
        if (top)
        {
            int j = i * 2;
            arr = j + arr;
            arr = arr - i;
            arr = arr - i;
            // Without pointer tracking we would not be able to track the offset here.
            char s = *(arr + tmp - i - i); // NOWARN
        }
        char s = *arr;       // NOWARN
        char s = *(arr + 1); // WARN
        char s = *(arr - 1); // WARN
        arr = arr + 1;
    }
}
int main()
{
    int len;
    top = rand() % 2;
    if (top)
        len = 5;
    else
        len = 10;
    char *ptr = malloc(len);
    readUntil(ptr, len);
}