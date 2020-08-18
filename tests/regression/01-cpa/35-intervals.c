// PARAM: --enable ana.int.interval --disable ana.int.def_exc --disable ana.int.enums
void main(){
  int n = 7;
  for (; n; n--) {
      assert(n==1); // UNKNOWN!
  }
  return;
}
