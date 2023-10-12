// PARAM: --set ana.activated[+] "localTraces"

void main() {
  int x = 1;
  int n = 1;
  int g = 1;
  while (++n < 5) {
    while (++g < 4) {
        x = x + 2147483646;  // WARN
    }
  }
}