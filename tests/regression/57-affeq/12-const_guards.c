//SKIP PARAM: --set ana.activated[+] affeq   --set ana.apron.domain "affeq" --enable ana.int.interval
int main() {
  int two = 2;
  int three = 3;
  int six = 6;

  int p = 0;

  if (two == three) {
      p = 1;
  }
  __goblint_check(p == 0);

  int k = 0;
  if (two + 3 == 5) {
      k = 1;
  }
  __goblint_check(k == 1);
}