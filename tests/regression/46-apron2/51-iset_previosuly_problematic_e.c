// SKIP PARAM: --enable ana.int.interval_set --set ana.activated[+] apron
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
// NOCHECK
int main(int argc, char const *argv[])
{
    int j = -1;
    int digits = 0;

    int hour12;
    int number_value;

    if (hour12 > 12) {
        hour12 -= 12;
    }

    digits = 0;

    while (j < 9)
    {
        number_value /= 10;
        j++;
    }

    return 0;
}
