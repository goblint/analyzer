// PARAM: --set ana.activated[+] "localTraces"
int global = 7;
void g(int k, int param) { global = k - param + global; }

void f(int param) {
  if (param < 2) {
    g(param, 5);
  }
  param = 7;
  return;
}
void main() {
  int param = -14;
  f(1);
  param = global;
}