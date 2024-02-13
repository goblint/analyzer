// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] apron
// This test checks the no overflow computation in apron
int main()
{
    unsigned int len = rand();
    len = len % 10;
    int top;
    for (int i = 0; 1 - (i - 1 < len) < 1; i++) // raises both both branch dead if no_overflow is wrong computed
    {
        int tmp = i;
    }

    return 0;
}
