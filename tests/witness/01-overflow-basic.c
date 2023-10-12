// PARAM: --set ana.activated[+] "localTraces"

void main() {
  int x = 1;
  if (x < 1) {
    x = 3;
  } else {
    x = x + 2147483647;  // WARN
  }
}