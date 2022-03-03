//SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_top --enable ana.int.interval

int main() {
    int x, y;
    int p = 0;

    if (x - 2 == __INT32_MAX__) {
      assert (x == __INT32_MAX__ + 2); //UNKNOWN!
      p = 1;
    }

    assert (p == 0); //UNKNOWN!

    if (x + y == __INT32_MAX__) {
        assert(1);
    }

}