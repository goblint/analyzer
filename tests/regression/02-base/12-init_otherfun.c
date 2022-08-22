// SKIP PARAM: --set otherfun "['f']"

int glob1 = 5;

int g() {
  __goblint_check(glob1 == 5);
  return 0;
}

int main() {
  __goblint_check(glob1 == 5);
  return 0;
}
