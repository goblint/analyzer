int main(void) {
  int x;
  int y;
  asm ("nop" : "=x" (y) : "x" (x));
  return y;
}
