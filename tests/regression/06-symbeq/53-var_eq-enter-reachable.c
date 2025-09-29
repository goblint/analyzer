// PARAM: --set ana.activated[*] var_eq
// CRAM

void foo() {
  // var_eq has x == y here
}

int main() {
  int x;
  int y;
  y = x;
  foo();
  return 0;
}
