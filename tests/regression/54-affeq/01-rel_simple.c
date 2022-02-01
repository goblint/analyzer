// SKIP PARAM: --set ana.activated[+] affeq  --set exp.apron.privatization dummy --set sem.int.signed_overflow assume_none
// Example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf
void main(void) {
    int i;
    int j;
    int k;
    i = 2;
    j = k + 5;

    while (i < 100) {
        i = i + 1;
        j = j + 3;
    }

    assert (j == 305);
}
