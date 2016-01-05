// PARAM: --set otherfun "['f']" --enable exp.earlyglobs

int glob;

void f() {
  int i = glob;
  assert(i == 0);
}

int main(void *arg) {
  return 0;
}
