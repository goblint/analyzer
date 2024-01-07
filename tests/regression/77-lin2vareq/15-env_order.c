//SKIP PARAM: --set ana.activated[+] lin2vareq  --set sem.int.signed_overflow assume_none
  int next;

int main() {
    next = 0;
    int t;
    next = t;

    if (next == 0) {
        t = 5;
    }

    __goblint_check(t == 5); //UNKNOWN!
}