#include <stdio.h>
#include <assert.h>

void unknown_fn(int *i);

int main(){
  int a = 1;
  int b = 1;
  int *x;
  int t;
  int rnd;

  // setup
  if (rnd)
    x = &a;
  else 
    x = &b;
  
  if (rnd){
    x = 0;
    assert(x == 0);
  }

  // tests
  
  // no information loss between int <-> T*
  assert((int)0 == (int*)0);

  // filtering out the null pointer possibility
  if (x){    
    // assert(x != 0);
    assert(*x = 1);
    b = 2;
    assert(*x != 0);
    b = 0;
    assert(*x == 0); // UNKNOWN
    if (x != &b) {
      assert(x == &a);
      assert(*x == 1);
    }
  } else {
    assert(x == 0);
    unknown_fn(x); // unknown fun warning but no unsound 
                   // or 0-ptr warning
  }
  
  unknown_fn(0);
  return 0;
}

