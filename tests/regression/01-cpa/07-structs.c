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
  __goblint_check(a.kaal == 5);

  // checking assignment of records
  b = a;
  __goblint_check(b.kaal == 5);

  // check that a and b are different
  a.kaal = 12;
  b.kaal = 13;
  __goblint_check(a.kaal == 12);

  // pointer to field
  p = & a.kaal;
  *p = 6;
  __goblint_check(a.kaal == 6);

  // pointer to struct
  kp = &a;
  __goblint_check(kp->kaal == 6);
  kp->hind = 200;
  __goblint_check(a.hind == 200);


  // wicked addresses
  if (k1) kp = &b; // kp -> {a,b}
  p = & kp->hind; // p -> {a.hind, b.hind}
  // and here we *add* some more addresses
  if (k2) p = &i;
  if (k2) p = & b.kaal;
  // p points to a.hind, b.hind, b.kaal and i, invalidate them!
  *p = 666;
  __goblint_check(a.hind == 666); // UNKNOWN!
  __goblint_check(b.hind == 666); // UNKNOWN!
  __goblint_check(b.kaal == 666); // UNKNOWN!
  __goblint_check(i      == 666); // UNKNOWN!


  // structs in structs
  a.hind = 13;
  a.kaal = 17;
  kalamaja.kala.hind = 5;
  __goblint_check(kalamaja.kala.hind == 5);
  kalamaja.kala = a;
  __goblint_check(kalamaja.kala.hind == 13);
  kalamaja.arv = 7;
  kalamaja.kala.kaal = 176;
  __goblint_check(kalamaja.kala.kaal == 176);
  __goblint_check(a.kaal != 176);

  // just some more testing
  mp = &kalamaja;
  __goblint_check(mp->kala.kaal == 176);
  p = &mp->kala.hind;
  *p = 47;
  b = kalamaja.kala;
  __goblint_check(b.hind == 47);

  // global struct
  __goblint_check(glob.kaal == 3);
  __goblint_check(glob.hind == 0);
  glob.hind = 5;
  __goblint_check(glob.hind == 5);
  glob = a;
  __goblint_check(glob.hind == 13);

  // invalidation
  a.hind = 1;
  a.kaal = 2;
  scanf("%d", &a.hind);
  __goblint_check(a.hind == 1); // UNKNOWN!
  __goblint_check(a.kaal == 2);

  return 0;
}
