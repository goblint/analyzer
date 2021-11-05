// PARAM: --disable ana.int.def_exc --enable exp.annotated.precision --set ana.int.refinement fixpoint

int f(int in) __attribute__((goblint_precision("def_exc"))) {
  return in + 1;
}

int main() {
  int a = 1;
  assert(a); // UNKNOWN!
  a = f(a);
  return 0;
}