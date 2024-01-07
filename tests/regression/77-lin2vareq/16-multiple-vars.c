//SKIP //PARAM: --set ana.activated[+] lin2vareq  --enable ana.int.interval

int f (int j) {
    return j + 1;
}
int main() {
    int test = f(10);
    __goblint_check(test == 11);
  }