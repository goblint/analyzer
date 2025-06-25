int main(void) {
  int *p;
  int i;
  int *q = p + i;
  *q = 42; // TODO WARN (may NULL dereference)
}
