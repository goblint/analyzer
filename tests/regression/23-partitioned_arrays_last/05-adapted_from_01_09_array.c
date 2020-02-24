// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --sets exp.partition-arrays.keep-expr "last" --set ana.activated "['base','expRelation']"
#include<stdio.h>
#include<assert.h>

int fun_5() { return 5; }
int fun_6() { return 6; }
int fun_5b() { return 5; }

struct kala {
  int a[5];
};

struct kass {
  int v;
};

int main () {
  int i,t, k1,k2,top;

  int a[] = {2,2,2};
  int b[2], c[3];
  int (*f[2])() = {fun_5, fun_6};
  int (*g[2])() = {fun_5, fun_5b};
  int (*fp)();
  int *ip;
  int (*iap)[];

  // really really top
  if (i) top = (int) &top;
  else   top = 5;

  assert(a[0] == 2);
  assert(a[1] == 2);
  assert(a[2] == 2);

  // writing to unknown index:
  // NB! We assume the index is in bounds!
  if (k1) i=0; else i=1;
  a[i] = 0;
  assert(a[0] == 0); // UNKNOWN
  assert(a[1] == 0); // UNKNOWN
  assert(a[2] == 0); // FAIL

  // reading from unknown index:
  b[0] = 2; b[1] = 2;
  assert(b[i] == 2);
  b[0] = 3;
  assert(b[i] == 2); // UNKNOWN

  // function arrays
  t = f[i]();
  assert(t == 5); // UNKNOWN
  t = g[i]();
  assert(t == 5);

  // array has set of addresses:
  if (k2) f[i] = fun_5b;
  t = f[1]();
  assert(t == 5); // UNKNOWN

  // now we collect all the sets:
  fp = f[i];
  t = fp();
  assert(t == 5); // UNKNOWN
  fp = g[i];
  t = fp();
  assert(t == 5);

  //  NASTY ARRAY OPS:
  c[0] = 5; c[1] = 5; c[2] = 5;
  // this is not usual: a pointer to an array (easy!)
  iap = &c;
  t = (*iap)[2];
  assert(t == 5);

  // Typical C: a pointer to first element of array (difficult!)
  ip = c; // this means &c[0]

  // dereferencing...
  assert(*ip == 5);

  // pointing into the array
  ip = &c[1];
  assert(*ip == 5);

  // and some pointer arithmetic (tests are meaningless)
  *ip = 6;
  ip++;
  assert(*ip == 5);

  // Now testing arrays inside structs.
  struct kala x;
  ip = x.a;
  x.a[0] = 7;
  assert(*ip == 7);

  // (typeless) Top index
  assert(x.a[top] == 7); // UNKNOWN

  // And finally array of structs
  struct kala xs[5];
  xs[0] = x;
  ip = &xs[0].a[0];

  struct kass k[1];
  k[0].v = 42;
  assert(k[0].v == 42);

  // multi-dim arrays
  int ma[1][1];
  ma[0][0] = 42;
  assert(ma[0][0] == 42);

  //i = hash("kala");
  //printf("Hash value: %d", i);

  // NB arrays must be in bounds... otherwise everything fails!
  // It's not possible to analyze this:
  // a[3] = 666;

  return 0;
}
