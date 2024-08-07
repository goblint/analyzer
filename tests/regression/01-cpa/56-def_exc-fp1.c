// PARAM: --enable ana.sv-comp.functions
// manually minimized from sv-benchmarks/c/seq-mthreaded/pals_lcr.3.ufo.UNBOUNDED.pals.c
// FIXPOINT: used to not reach fixpoint due to def_exc range
_Bool __VERIFIER_nondet_bool(void) ;

_Bool mode1  ;
void node1(void)
{
  if (mode1)
    mode1 = 0;
  else
    mode1 = 1;
}

int main(void)
{
  mode1 = __VERIFIER_nondet_bool();
  while (1)
    node1();
  return 0;
}
