// SKIP PARAM: --set otherfun "['f']" --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper','mutex','access','assert']"

int glob1 = 5;

int g() {
  __goblint_check(glob1 == 5);
  return 0;
}

int main() {
  __goblint_check(glob1 == 5);
  return 0;
}
