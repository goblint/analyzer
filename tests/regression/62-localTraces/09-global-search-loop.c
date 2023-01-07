// PARAM: --set ana.activated[+] "localTraces"

int x = 3;
void main() {
  int k = 2;
  while (x < 6) {
    k = 3;
    x = x + 1;
  }
  k = x;
}