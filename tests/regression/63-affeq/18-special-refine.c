//SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_top --enable ana.int.interval

int main() {
    int top;
    int blarg = top;

    if(top == 8) {
      __goblint_check(top == 8);
      __goblint_check(top == blarg);
    }
}
