// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --set ana.specification 'CHECK( init(main()), LTL(G ! call(reach_error())) )' --enable ana.int.interval
// previously fixpoint not reached
// extracted from sv-benchmarks loops-crafted-1/loopv2
int SIZE = 50000001;
int __VERIFIER_nondet_int();
int main() {
  int n,i,j;
  n = __VERIFIER_nondet_int();
  if (!(n <= SIZE)) return 0;
  i = 0; j=0;
  while(i<n){

    i = i + 4;
    j = i +2;
  }
  return 0;
}
