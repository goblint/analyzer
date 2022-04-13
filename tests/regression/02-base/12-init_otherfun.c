// SKIP PARAM: --set otherfun "['f']" --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper','mutex','access']"

int glob1 = 5;

int g() {
  assert(glob1 == 5);
  return 0;
}

int main() {
  assert(glob1 == 5);
  return 0;
}
