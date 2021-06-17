// PARAM: --sets solver td3 --enable ana.int.congruence --disable ana.int.def_exc
int main(){
    // A refinement of a congruence class should only take place for the == and != operator.
    int i;
    if (i==0){
      assert(i==0);
    } else {
      assert(i!=0); //UNKNOWN!
    }

    int j;
    if (j != 0){
      assert (j != 0); //UNKNOWN!
    } else {
      assert (j == 0);
    }

    int k;
    if (k > 0) {
      assert (k == 0); //UNKNOWN!
    } else {
      assert (k != 0); //UNKNOWN!
    }

    return 0;
}