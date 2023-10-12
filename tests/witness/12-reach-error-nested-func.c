// PARAM: --set ana.activated[+] "localTraces"

void reach_error() {}

int f(int param) {
  if (param < 2) {
    int x = f(5);
  }
  if( param < 100) { reach_error();}
  return param;
}
void main() {
  int param = -14;
  f(1);
}