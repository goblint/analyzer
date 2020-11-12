// SKIP PARAM: --sets otherfun "['f']" --set ana.activated "['base','threadid','threadflag','escape']"

int glob1 = 5;

int g() {
  assert(glob1 == 5);
  return 0;
}

int main() {
  assert(glob1 == 5);
  return 0;
}
