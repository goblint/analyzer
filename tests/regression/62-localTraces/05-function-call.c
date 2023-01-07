// PARAM: --set ana.activated[+] "localTraces"

int f(int x) {
  x = 10;
  return x - 4;
}

void main() {
  int x = 7;
  int y = f(12);
  x = 3;
}