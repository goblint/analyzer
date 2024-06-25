// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts

int a;
long b, c, d, e, f, g, h;
int *i;
k() {
  int j;
  long top;
  while (top) {
    b = a * 424;
    c = j;
    d = j + b;
    e = a * 424;
    f = e + 8;
    g = j;
    h = j + f;
    i = h;
    a = a + 1;
    __goblint_check(g == c);
    // __goblint_check(h == 8 + d);
    __goblint_check((long)i == h);
    __goblint_check(j == c);
  }
}
main() { k(); }
