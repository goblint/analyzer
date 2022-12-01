//SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_top --enable ana.int.interval

int main() {
    int x, y;
    int p = 0;

    if (x - 2 == __INT32_MAX__) {
      __goblint_check(x == __INT32_MAX__ + 2); //UNKNOWN!
      p = 1;
    }

    __goblint_check(p == 0); //UNKNOWN!

    if (x + y == __INT32_MAX__) {
        __goblint_check(1);
    }

}