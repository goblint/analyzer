// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned
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
