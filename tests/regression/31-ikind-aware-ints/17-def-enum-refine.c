//PARAM: --set ana.int.refinement once  --enable ana.int.enums
int main() {
  int x;
  _Bool c;
  if(c) { x--;}
  else { x--;}
  // FIXPOINT: The veryfier claimed that the fixed-point was not reached here due to a bug in Enums.leq
  // The leq wrongly returned false for the Enums {0} and not{}[0,1]
  return 0;
}
