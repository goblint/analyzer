// SKIP PARAM: --sets solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','assert','octApron']"
// Example from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/recursive-simple/afterrec-1.c
void f(int n) {
  if (n<3) return;
  n--;
  f(n);
  assert(1);
}

int main(void) {
  f(4);
}
