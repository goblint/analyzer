// PARAM: --set ana.activated[+] affeq  --set sem.int.signed_overflow assume_none
  int next;

int main() {
    next = 0;
    // Due to a bug in the dim add function, t was always set to 0 in the following block
    int t;
    next = t;

    if (next == 0) {
        t = 10;
    }

    assert (t == 10); //UNKNOWN!
}
