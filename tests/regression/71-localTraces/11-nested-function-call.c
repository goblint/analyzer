// PARAM: --set ana.activated[+] "localTraces"

void f(int param) {
  if (param < 2) {
    f(5);
  }
  param = 7;
  return;
}
void main() {
  int param = -14;
  f(1);
}