// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] apron
// FIXPOINT: These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
int main(int argc, char const *argv[])
{
    int l;
    int r = 42;

    while(1) {
        if (l-r <= 0) {
            r--;
        } else {
            break;
        }
    }

   return 0;
}
