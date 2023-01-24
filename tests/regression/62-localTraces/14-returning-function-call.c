// PARAM: --set ana.activated[+] "localTraces"
int global = 7;
int f() { return global - 5; }

void main() {
  global = 1;
  int x = f();
}