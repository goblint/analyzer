// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval --set solver slr3tp
#include <assert.h>

int main() {
  short a;
  a = a % 10;
  int b;
  int c;
  b = a + 1;
  c = a + 2;
	int x, g;
	
  for(x=0; x < 50; x++){
		g = 1;
  }
  b = a + x;
	
	// x = [50, 50] after narrow
  if(b - a > 50){ // live after widen, but dead after narrow
    // node after Pos(x>50) is marked dead at the end
    // but the loop is not with x = [51,2147483647]
    for(int i=0; i<=0 && i > -1000; i--){
      b = 8;
    }
    assert(1); // NOWARN (unreachable)
  }
  __goblint_check(b  == c + 48);
  return 0;
}
