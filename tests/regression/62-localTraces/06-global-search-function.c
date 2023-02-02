// PARAM: --set ana.activated[+] "localTraces"

int y = 1;

int f(int x) {
  x = y;
  return x - 4;
}

void main() {
  int x = 7;
  y = 9;
  y = f(12);
  x = 3;
  int k = y;
}