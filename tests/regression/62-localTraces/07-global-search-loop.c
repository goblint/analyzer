// PARAM: --set ana.activated[+] "localTraces"

int y = 10;

void main() {
  int x = 3;
  int k = 1;
  while (x < 6) {
    k = y;
    y = x + x;
    x++;
  }
}