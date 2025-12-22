// CRAM PARAM:  --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --enable ana.int.bitfield
#define a (2)
unsigned int __VERIFIER_nondet_uint();

int main() {
  unsigned int sn=0;
  unsigned int loop1=__VERIFIER_nondet_uint(), n1=__VERIFIER_nondet_uint();
  unsigned int x=0;

  while(1){
    sn = sn + a;
    x++;
  }
}

