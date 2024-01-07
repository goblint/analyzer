//SKIP PARAM: --set ana.activated[+] lin2vareq

int main() {

    unsigned int a = 1;

    unsigned int b = -a;

    __goblint_check(b == 4294967295);

    unsigned short int allbits = -1;

    short int signedallbits = allbits;

    __goblint_check(signedallbits == -1);

    short c = 32767;
    c = c + 2;

    __goblint_check(c == -32767);
}