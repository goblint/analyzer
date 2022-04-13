// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --enable exp.fast_global_inits  --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper']" --set ana.base.privatization none
// Without fast_global_inits this takes >150s, when it is enabled < 0.1s
int global_array[50][500][20];

int main(void) {
  for(int i =0; i < 50; i++) {
      assert(global_array[i][42][7] == 0);
  }
}
