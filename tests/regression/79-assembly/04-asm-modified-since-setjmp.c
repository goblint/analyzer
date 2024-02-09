#include <setjmp.h>

jmp_buf buf;

int main(void) {
  int x;
start:
  x = 5;
  if (setjmp(buf)) {
    //read
    asm ("nop" : : "x" (x));
    return 0;
  } else {
    //write
    asm goto ("nop" : "=x" (x) : : : exit2, exit); //WARN
    longjmp(buf, 1);
  }
exit2:
exit: return 0;
}
