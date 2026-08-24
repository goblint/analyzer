// PARAM: --enable ana.int.interval
// NOCRASH
int main()
{
    unsigned int  l = 0;

    l = 0;

    if(l%2U)
        l = 5;

    if (!(l % 2U))
        l = l+1;

    return 0;
}
