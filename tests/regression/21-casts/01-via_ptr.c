#include <stdio.h>
#include <assert.h>
#include <limits.h>

typedef void* addr;
struct a { int x; int y; };
struct b { struct a x; int y; };

int main(){
  // # ints
  {
    // normal cast
    int a = 128;
    typedef signed char schar; // signed may not be the default!
    schar b = a; // downcast resulting in overflow (pos to neg)
    assert(b == -128);
    unsigned char b2 = -1; // neg to pos
    assert(b2 == 255);
    // via pointer (same data reinterpretated with different type)
    // downcasts are ok for pos. values
    schar* c = (schar*) &a;
    b = *c;
    assert(b == -128);
    // and also for neg. values:
    a = -1;         b = *c; printf("a: %d, b: %d\n", a, b); assert(b == -1);
    a = INT_MIN+1;  b = *c; printf("a: %d, b: %d\n", a, b); assert(b == 1);
    a = CHAR_MIN;   b = *c; printf("a: %d, b: %d\n", a, b); assert(b == -128);
    a = CHAR_MIN-1; b = *c; printf("a: %d, b: %d\n", a, b); assert(b == 127);
    // upcast must always lead to top since we might read garbage (except we know there was a corresponding downcast before)
    {
      schar a = 1;
      schar b = 1;
      int* pa = (int*) &a;
      int* pb = (int*) &b;
      printf("a: %d, b: %d\n", *pa, *pb);
      assert(*pa != *pb); // UNKNOWN!
    }

    // check that limits are handled correctly
    long l = LONG_MAX;
    unsigned long ul = ULONG_MAX;
    long long ll = LLONG_MAX;
    unsigned long long ull = ULLONG_MAX;
    printf("l: %ld, ul: %lu, ll: %lld, ull: %llu\n", l, ul, ll, ull);
    l = LONG_MAX+1;
    ul = ULONG_MAX+1;
    ll = LLONG_MAX+1;
    ull = ULLONG_MAX+1;
    printf("l: %ld, ul: %lu, ll: %lld, ull: %llu\n", l, ul, ll, ull);
  }


  // # structs
  {
    // pointer to struct == pointer to first field

    // cast in (add first field)
    struct a a;
    a.x = 3;
    assert(a.x == 3);
    assert(*((int*) &a) == 3);
    assert((&a)[0].x == 3);
    *((int*) &a) = 5;
    assert(a.x == 5);
    // two levels
    struct b b;
    b.x = a;
    assert(b.x.x == 5);
    ((struct a*) &b)->x = 6;
    assert(b.x.x == 6);
    *((int*) &b) = 7;
    assert(b.x.x == 7);

    // cast out (strip first field)
    a.y = 8;
    assert(((struct a*) &a.x)->y == 8);
    b.y = 9;
    assert(((struct b*) &b.x.x)->y == 9);

    // there are no assumptions one can make for following fields!
    assert(*((&a.x)+1) == 8); // UNKNOWN!
    // pointers into the middle that are casted out must be top!
    // this gives a segfault:
    assert(((struct a*) &a.y)->y == 8); // UNKNOWN!
  }


  // # arrays
  {
    int c[5];
    int *e;
    e = c;
    assert(c == &c[0]);
    assert(c == e);
    assert(&c[1] == e+1);
    // assert(c[1] == *(e+1)); // TODO array content is still `Bot
    // of structs
    struct a f[5];
    assert((int*)f == &(f[0].x)); // index 0, first field
    assert((void*)f == &(f[0].x)); // same, but with other type (address is still the same)
    // same, with typedef:
    addr x, y;
    x = (addr)f;
    y = &f[0].x;
    assert(x == y);
    int* p = (int*)f;
    assert((((struct a*)p)+1) == &f[1]);
    // of arrays
    int g[3][4];
    p = (int*)g;
    assert(((int(*)[4])p)+1 == &g[1]);
  }

  return 0;
}
