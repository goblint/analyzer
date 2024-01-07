//SKIP PARAM: --set ana.activated[+] lin2vareq  --set sem.int.signed_overflow assume_none
  int next; // global variables are initialized to 0

int main() {
    int t;

    if (next == 0) {
        t = 5;
    }

    __goblint_check(t == 5); //SUCCESS
}
