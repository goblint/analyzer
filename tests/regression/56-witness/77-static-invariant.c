// CRAM

static int static_global = 1;

void foo() {
  static int static_local = 2;
  {
    static int static_local_nested = 3;
  }
}

int main() {
  foo();
  return 0;
}
