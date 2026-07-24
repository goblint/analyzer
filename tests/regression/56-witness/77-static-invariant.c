// CRAM

static int static_global = 1;

void foo() {
  static int static_local = 2;
}

int main() {
  foo();
  return 0;
}
