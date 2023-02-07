// PARAM: --set ana.activated[+] "localTraces"

int f(int x) {
  x = 10 - x;
  return x - 4;
}

void main() {
  int x = 7;
  int y = f(x);
  x = 3;
}