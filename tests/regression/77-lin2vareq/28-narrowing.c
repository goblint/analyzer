// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval
#include <assert.h>

int g = 0;

int main() {
  int x;

  for (x = 0; x < 50; x++) {
    g = 1;
  }

  if (x > 50) { 
    
    for (int i = 0; i <= 0; i--) {
      g = 57;

      int y;

      for (y = 0; y < x; y++) { 
        g = 42;
      }
    }
    assert(1); // NOWARN (unreachable)
  }
}
