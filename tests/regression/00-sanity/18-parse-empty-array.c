// Test for issue in https://github.com/goblint/cil/issues/19
static int a[];
static int a[] = {};
static int b[0] = {};

void main(void) {
  int a[] = {};
}
