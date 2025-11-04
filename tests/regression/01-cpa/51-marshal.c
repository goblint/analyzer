// PARAM: --enable ana.int.interval
// NOCHECK
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
