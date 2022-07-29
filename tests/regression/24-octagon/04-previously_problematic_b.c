// SKIP PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.activated[+] apron --set ana.base.privatization none
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
typedef int wchar_t;
typedef unsigned long size_t;

char *trim2(char const *s, int how)
{
    char *d;
    char *tmp___4;
    unsigned int state;
    int tmp___9;

    if (tmp___9 == 0) {
        tmp___9 = 1;
    }

    size_t tmp___18;

    d = tmp___4;

    if (tmp___18 > 1UL)
    {
        if (how != 1)
        {
            state = 0U;

            while (1)
            {
                if (!tmp___9){
                }
                else {
                    break;
                }


                    state = 1U;
            }
        }
    }
    return (d);

}

int main(int argc, char const *argv[])
{
    char *s;
    trim2(s, 4);

    return 0;
}
