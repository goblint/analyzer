// PARAM: --set ana.activated[+] "localTraces"

int global = 5;

void main() {
  int x = 7;
  int k = 1;
  while (k < 4) {
    x = k - global;
    k++;
    global -= 1;
  }
  k = global;
}