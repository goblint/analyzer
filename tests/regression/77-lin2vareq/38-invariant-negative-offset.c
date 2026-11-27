// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.sv-comp.functions
// CRAM: check the witness at the join.

extern int __VERIFIER_nondet_int(void);

int main(void) {
  unsigned int x, y;
  if (__VERIFIER_nondet_int()) {
    x = 1;
    y = 0;
  } else {
    x = 2;
    y = 1;
  }
  return 0;
}
