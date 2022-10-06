// https://github.com/goblint/analyzer/issues/554
// FIXPOINT
int a;

int main() { c(&a); }
int c(int *f) {
  unsigned long d;
  int *e = &a;
  d = 0;
  while (1) {
    e = f++;
    d++;
  }
}
