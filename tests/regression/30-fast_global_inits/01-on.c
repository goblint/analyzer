// PARAM: --sets solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled --set ana.activated "['base','expRelation']"
// This checks that partitioned arrays and fast_global_inits are no longer incompatible
int global_array[50];

int main(void) {
  for(int i =0; i < 50; i++) {
      assert(global_array[i] == 0);
  }
}
