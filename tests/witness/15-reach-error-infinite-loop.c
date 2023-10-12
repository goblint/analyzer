// PARAM: --set ana.activated[+] "localTraces"

void reach_error() {}

void main() {
  int x = 3;
  int y = 4;
  while (y < 7) {
    x = x + 1;
    if( x < 15 ) { reach_error(); }
  }
}