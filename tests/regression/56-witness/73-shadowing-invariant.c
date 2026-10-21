// CRAM PARAM: --set ana.activated[+] var_eq

int foo = 1;

int main() {
  int foo = 2; // shadows global

  int bar = 3;
  {
    int bar = 4; // shadows outer block
    return 0;
  }
}
