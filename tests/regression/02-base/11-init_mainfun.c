// SKIP PARAM: --set otherfun "['f']"

int glob;

void f() {
  int i = glob;
  assert(i == 0);
}

int main(void *arg) {
  return 0;
}
