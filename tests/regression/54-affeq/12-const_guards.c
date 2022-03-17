//SKIP PARAM: --set ana.activated[+] affeq   --set ana.apron.domain "affeq" --enable ana.int.interval
int main() {
  int two = 2;
  int three = 3;
  int six = 6;

  int p = 0;

  if (two == three) {
      p = 1;
  }
  assert(p == 0);

  int k = 0;
  if (two + 3 == 5) {
      k = 1;
  }
  assert(k == 1);
}