// SKIP PARAM: --sets ana.activated[+] apron --enable ana.int.interval
int main(void) {
  unsigned int x = 10;

  while (x >= 10) {
    x += 2;
  }

  assert(1);
}
