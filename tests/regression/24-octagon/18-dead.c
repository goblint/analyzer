//SKIP PARAM: --enable ana.int.interval --set ana.activated[+] apron
// NOCRASH (was arithmetic on bottom)
int main(void)
{
    int a;
    unsigned short b;
    int tmp;

    b = (unsigned short )a;

    if ((int )b + (int )b < (int )b) {

        tmp = (int )b;
        b += 1;

        if (! tmp) {
           tmp = 1;
        }

    }

    return 0;
}
