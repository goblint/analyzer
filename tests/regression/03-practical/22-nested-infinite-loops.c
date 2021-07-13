// https://github.com/goblint/analyzer/issues/231#issuecomment-868369123
int main(void) {
  int x = 0;
  while(1) {
    while(1) {
      x++;
    }
    x--;
  }
  x--;
  return x;
}