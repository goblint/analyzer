#include<stdio.h>
#include<assert.h>

int fun_5() { return 5; }
int fun_6() { return 6; }
int fun_5b() { return 5; }

int main () {
  int i,t, k1,k2;

  int a[] = {1,2,3};
  int b[2], c[3];
  int (*f[2])() = {fun_5, fun_6};
  int (*g[2])() = {fun_5, fun_5b};
  int (*fp)();
  int *ip;
  int (*iap)[];

  assert(a[0] == 1);
  assert(a[1] == 2);
  assert(a[2] == 3);
  
  // writing to unknown index:
  // NB! We assume the index is in bounds!
  if (k1) i=0; else i=1;
  a[i] = 2;
  assert_unknown(a[0]);
  assert(a[1] == 2);
  assert_unknown(a[2]);

  a[0] = 2; a[2] = 3;
  if (k1 == 2)
    assert(a[k1] == 3);
  else
    assert(a[k1] == 2);

  // Unitialized arrays
  b[0] = 2;
  assert(b[0] == 2);
  assert_unknown(b[1]);
  
  // reading from unknown index:
  b[0] = 2; b[1] = 2;
  assert(b[i] == 2);
  b[0] = 3;
  assert_unknown(b[i]);

  // function arrays
  t = f[0]();
  assert(t  == 5);
  t = f[1]();
  assert(t  == 6);
  t = f[i]();
  assert_unknown(t);
  t = g[i]();
  assert(t == 5);

  // array has set of addresses:
  if (k2) f[i] = fun_5b;
  t = f[0]();
  assert(t == 5);
  t = f[1]();
  assert_unknown(t);

  // now we collect all the sets:
  fp = f[i];
  t = fp();
  assert_unknown(t);
  fp = g[i];
  t = fp();
  assert(t == 5);

  // arrays get invalidated...
  // this is a problem:
  a[i] = 7;
  a[0] = 3;
  assert(a[0] == 3);


  //  NASTY ARRAY OPS:
  c[0] = 5; c[1] = 7; c[2] = 13;
  // this is not usual: a pointer to an array (easy!)
  iap = &c;
  t = (*iap)[2];
  assert(t == 13);

  // Typical C: a pointer to first element of array (difficult!)
  ip = c; // this means &c[0]
  assert(ip[0] == 5);
  assert(ip[1] == 7);
  assert(ip[2] == 13);

  // dereferencing...
  *ip = 6;
  assert(c[0] == 6);

  // pointing into the array
  ip = &c[1];
  assert(ip[0] == 7);
  assert(ip[1] == 13);
  
  // and some pointer arithmetic
  *ip = 8;
  ip++;
  *ip = 14;
  assert(c[0] = 6);
  assert(c[1] = 8);
  assert(c[2] = 13);


  //i = hash("kala");
  //printf("Hash value: %d", i);

  // NB arrays must be in bounds... otherwise everything fails!
  // It's not possible to analyze this:
  // a[3] = 666;

  return 0;
}
