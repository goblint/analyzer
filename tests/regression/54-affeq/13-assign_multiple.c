//SKIP //PARAM: --set ana.activated[+] affeq  --enable ana.int.interval
int f (int j) {
    return j + 1;
}
int main() {
    int test = f(10);
    assert (test == 11);
  }