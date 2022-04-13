// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.activated "['base','threadid','threadflag','expRelation','mallocWrapper']" --set ana.base.privatization none
void callee(int j) {
  j++;
}

int main(void) {
  int x = 3;
  int y = 2;
  int z = x + y;
  callee(1);
  return 0;
}
