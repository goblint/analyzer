// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts

int *a, b;
c() { b = 0; }
main() {
  int *d;
  if (a == d)
    ;
  else
    __goblint_check(a != d);
  c();
}
