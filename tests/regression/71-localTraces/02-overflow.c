// PARAM: --set ana.activated[+] "localTraces"

void main() {
  int unknown = 0;
  int x = 1;
  if (unknown) {
    x = 3;
  } else {
    x = x + 2147483647;
  }
}