// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts
int a, b, c;
void *d(const *e) { return e + 200; }
int *f() {}
main() {
  g(a, c, b);
  if (0) {
    __goblint_check(0); // NOWARN (unreachable)
  }
  __goblint_check(1); // reachable
}
g(int, struct h *, struct i *) {
  int *j = f();
  d(j);
  __goblint_check(1); // reachable
}
