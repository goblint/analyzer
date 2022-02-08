//SKIP PARAM: --set ana.activated[+] affeq

int main() {

   unsigned int x  = 1;

    unsigned int y = -x;

    assert(y == 4294967295);

    unsigned short int allbits = -1;

    short int signedallbits = allbits;

    assert (signedallbits == -1);

    short s = 32767;
    s = s + 2;

    assert (s == -32767);
}
