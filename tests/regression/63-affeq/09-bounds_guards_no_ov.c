//SKIP PARAM:  --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none

int main() {
    int x, y;
    int p = 0;

    if (x - 2 == __INT32_MAX__) {
      p = 1;
    }

    __goblint_check(p == 0);

    if (x + y == __INT32_MAX__) {
        __goblint_check(x + y == __INT32_MAX__);
    }

}