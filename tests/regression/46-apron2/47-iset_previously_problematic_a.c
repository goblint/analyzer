// SKIP PARAM: --enable ana.int.interval_set --set ana.activated[+] apron
// These examples were cases were we saw issues of not reaching a fixpoint during development of the octagon domain. Since those issues might
// resurface, these tests without asserts are included
// NOCHECK
int main(int argc, char const *argv[])
{
    int top;

    int pad = 0;
    int to_uppcase;
    int change_case = 0;

    while (change_case != 1 && to_uppcase != 0) {
        if(top == 1) {
            to_uppcase = 1;
            continue;
        }

        if(top == 2) {
            change_case = 1;
            continue;
        }

        break;
    }

    return 0;
}
