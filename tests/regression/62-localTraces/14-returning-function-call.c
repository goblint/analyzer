// PARAM: --set ana.activated[+] "localTraces"
int global = 7;
int f() {
  int l = global - 5;
  return l;
}

void main() {
  global = 1;
  int x = f();
}