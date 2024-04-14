// PARAM: --set ana.context.callString_length 10 --set "ana.activated[+]" call_site --set ana.ctx_sens "['call_site']"  --enable ana.int.interval_set
// Will result in an endless loop without context insensitive analysis

int num_iterat = 2;

// main -> main -> ...
// [main, main, ...]
int main(void)
{
    if (num_iterat > 0)
    {
        num_iterat++;
        int res = main();
        __goblint_check(res == 5); // UNKNOWN
        return res;
    }
    else
    {
        if (num_iterat == 0)
        {
            return 5;
        }
        return 2;
    }
}