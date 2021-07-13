// PARAM: --sets solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','mallocWrapper','assert']"
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
