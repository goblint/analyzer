//SKIP //PARAM: --set ana.activated[+] affeq  --enable ana.int.interval
// An issue with the assignment of multiple vars made the assert evaluate to unknown
int f (int j) {
    return j + 1;
}
int main() {
    int test = f(10);
    assert (test == 11);
  }