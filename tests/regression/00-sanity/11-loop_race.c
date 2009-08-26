// PARAM: --allfuns

/*extern void g(void);*/

int x;

void f() {
  int i;
  while (1) {
    x++; // RACE!
  }
  i = 0;
}

int main () {
  return 0;
}
