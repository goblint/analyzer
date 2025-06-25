int main(void) {
  int *p;
  int i;
  int *q = p + i;
  *q = 42; // WARN (may NULL dereference)
}
