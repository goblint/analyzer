//SKIP PARAM: --set ana.activated[+] affeq

int main() {

   unsigned int x  = 1;

    unsigned int y = -x;

    __goblint_check(y == 4294967295);

    unsigned short int allbits = -1;

    short int signedallbits = allbits;

    __goblint_check (signedallbits == -1);

    short s = 32767;
    s = s + 2;

    __goblint_check(s == -32767);
}
