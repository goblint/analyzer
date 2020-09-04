#include <stdio.h>
#include <assert.h>

int main () {
  char a;
  // a = 127;
  if(a == 127) return;
  char b = a+1; // (char)128 = -128
  printf("b: %d\n", b);
  assert(b != -128); // UNKNOWN
  int c;
  if (c == -128) return; // c is not -128
  c = (char) c; // actual downcast: c could be 128, cast to char = -128
  assert(c != -128); // UNKNOWN!

  int top;
  unsigned long ul = 77777777;

  if(top) {
    ul = 255;
  }

  if(ul != 511) {
      unsigned char uc = (unsigned char)ul;
      assert(uc != 255); //UNKNOWN!
      ul = 1;
  }
}
