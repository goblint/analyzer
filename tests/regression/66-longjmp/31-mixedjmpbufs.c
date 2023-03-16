#include <pthread.h>
#include <goblint.h>
#include <setjmp.h>
#include <stdio.h>

jmp_buf error0;
jmp_buf error1;


int blorg(int x) {
  if(x > 8) {
    longjmp(error1, 1); // WARN (modified since setjmp)
  }

  return x;
}

int blub(int x,int y) {
  if(x == 0) {
    longjmp(error0, 1); // WARN (modified since setjmp)
  }

  return blorg(x-27+3);
}



int main(void) {

  if(setjmp(error0)) {
    printf("error0 occured");
    return -1;
  }

  if(setjmp(error1)) {
    printf("error1 occured");
    return -2;
  }

  int x, y;
  scanf("%d", &x);
  scanf("%d", &y);
  int x = blub(x, y); // NOWARN
  printf("%d", x);

  return 0;
}
