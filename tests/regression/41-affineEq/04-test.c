// SKIP PARAM: --set ana.activated[+] affeq  --set exp.apron.privatization dummy
void main(void) {

    int i = 0;

    int p = 0;
    if (i != 1) {
        assert (i != 1);
        p = 1;
    } else {
        p = 1000;
    }
    assert (p == 1);

}
