// PARAM: --enable ana.arrayoob
// extracted from SV-COMP task ldv-memsafety/ArraysOfVariableLength2.c

int bar(int b[]) {
  return 0;
}

void foo(int n) {
  int a[n];
  bar(a); //NOWARN
}

int main(void) {
  foo(0);
  return 0;
}
