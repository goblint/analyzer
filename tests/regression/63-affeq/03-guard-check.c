//SKIP PARAM:  --set ana.activated[+] affeq --set ana.relation.privatization top --set sem.int.signed_overflow assume_none
void main(void) {

    int i = 0;
    int k = 0;
    int z = i + k;

    int x, y;
    int f = x + y;

    //EQ
    if (x == 0) {
        __goblint_check(x == 0);
    }

    if (i == 0) {
        __goblint_check(1); // reachable
    }
    __goblint_check(2 * z == 2 * i + 2 * k);
    __goblint_check(2 * f == 2 * x + 2 * y);

    //DISEQ
    if (i != 1) {
       __goblint_check(1); // reachable
    }

    //SUP
    if (i > -1) {
        __goblint_check(1); // reachable
    }
    __goblint_check(f > x + y); //FAIL
    __goblint_check(z > i + k); //FAIL

    //SUPEQ
    if (i >= -1) {
        __goblint_check(1); // reachable
    }
    __goblint_check(f < x + y); //FAIL
    __goblint_check(f >= x + y);
    __goblint_check(z >= i + k + 5); //FAIL

}
