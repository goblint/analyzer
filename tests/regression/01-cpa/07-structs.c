#include<stdio.h>
#include<assert.h>

struct kala {
  int kaal;
  int hind;
} glob = {3};

struct maja {
  struct kala kala;
  int arv;
};

struct s {
	int i;
};

struct s l[1]; // array of struct

int main () {
  int i=0, *p, k1, k2, k3;
  struct kala a, b = {2,3}, *kp;
  struct maja kalamaja, *mp;

  a.kaal = 5;
  a.hind = 300;
  assert(a.kaal == 5);

  // checking assignment of records
  b = a;
  assert(b.kaal = 5);

  // check that a and b are different
  a.kaal = 12;
  b.kaal = 13;
  assert(a.kaal == 12);

  // pointer to field
  p = & a.kaal;
  *p = 6;
  assert(a.kaal == 6);

  // pointer to struct
  kp = &a;
  assert(kp->kaal == 6);
  kp->hind = 200;
  assert(a.hind == 200);


  // wicked addresses
  if (k1) kp = &b; // kp -> {a,b}
  p = & kp->hind; // p -> {a.hind, b.hind}
  // and here we *add* some more addresses
  if (k2) p = &i;
  if (k2) p = & b.kaal; 
  // p points to a.hind, b.hind, b.kaal and i, invalidate them!
  *p = 666;
  assert(a.hind == 666); // UNKNOWN!
  assert(b.hind == 666); // UNKNOWN!
  assert(b.kaal == 666); // UNKNOWN!
  assert(i      == 666); // UNKNOWN!


  // structs in structs
  a.hind = 13;
  a.kaal = 17;
  kalamaja.kala.hind = 5;
  assert(kalamaja.kala.hind == 5);
  kalamaja.kala = a;
  assert(kalamaja.kala.hind == 13);
  kalamaja.arv = 7;
  kalamaja.kala.kaal = 176;
  assert(kalamaja.kala.kaal == 176);
  assert(a.kaal != 176);
  
  // just some more testing
  mp = &kalamaja;
  assert(mp->kala.kaal == 176);
  p = &mp->kala.hind;
  *p = 47;
  b = kalamaja.kala;
  assert(b.hind == 47);

  // global struct
  assert(glob.kaal == 3);
  assert(glob.hind == 0);
  glob.hind = 5;
  assert(glob.hind == 5);
  glob = a;
  assert(glob.hind == 13);

  // invalidation
  a.hind = 1;
  a.kaal = 2;
  scanf("%d", &a.hind);
  assert(a.hind == 1); // UNKNOWN!
  assert(a.kaal == 2);

  return 0;
}
