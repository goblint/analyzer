// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval  --set sem.int.signed_overflow assume_none
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
    a = 1;
  }

  if (x > 50) { 
    
    for (int i = 0; i <= 0; i--) {
      c = 57;

      int y;

      for (y = 0; y < x; y++) { 
        b = 42;
      }
    }
    assert(0); // NOWARN (unreachable)
  }
  assert(b + 1 == c);// SUCCESS 
}
