// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval 
#include <assert.h>

int main() {
  short a;
  a = a % 10;
  int b;
  int c;
  b = a + 1;
  c = a + 2;
  int x;

  for (x = 0; x < 50; x++) {
    x = x+1;
  }

    
  for (int i = 0; i <= 50; i++) {
    x = 57;

    int y;

    for (y = 41 + a; y < i; y++) { 
      b = 42;
    }
  }
  assert(b + 1 == c);// SUCCESS 
  return 0;
}
