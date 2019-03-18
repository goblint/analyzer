#include<stdio.h>
#include<assert.h>

int fun_5() { return 5; }
int fun_6() { return 6; }
int fun_5b() { return 5; }

struct kala {
  int i;
  int a[5];
};

struct kalaw {
  int* a;
};

struct kass {
  int v;
};

union uArray {
  int a[5];
  int b[5];
};

union uStruct {
  int b;
  struct kala k;
};

void interesting(void) {
  // --------- PREPERATION --------------
  int top, j;
  
  // really really top 
  if (j) top = (int) &top;
  else   top = 5;

  // --------------- TEST 1 -------------
  struct kala l;
  int i = 0;

  while(i < 5) {
    l.a[i] = 42;
    i++;

    // Check assertion that should only hold later does not already hold here
    assert(l.a[4] == 42); //UNKNOWN
  }

  // Check the array is correctly initialized
  assert(l.a[1] == 42);
  assert(l.a[2] == 42);
  assert(l.a[3] == 42);
  assert(l.a[4] == 42);

  // Destructively assign to i
  i = top;

  // Check the array is still known to be completly initialized
  assert(l.a[1] == 42);
  assert(l.a[2] == 42);
  assert(l.a[3] == 42);
  assert(l.a[4] == 42);

  // -------------- TEST 2 ---------------
  struct kala kalas[5];

  int i2 = 0;

  while(i2 < 4) {
    int j2 = 0;
    while(j2<5) {
      kalas[i2].a[j2] = 8;
      j2++;
    }
    i2++;
  }

  // Initialization has not proceeded this far
  assert(kalas[4].a[0] == 8); //UNKNOWN

  assert(kalas[0].a[0] == 8); 


 /** 
  int *ip;
  int top;
  
  // And finally array of structs
  struct kala x;
  ip = x.a;
  x.a[0] = 7;
  assert(*ip == 7);
  
  // (typeless) Top index
  // this assert was in here from the previous array domain, but actually asserts
  // sth that is unsound, therefore the UNKNOWN was added
  assert(x.a[top] == 7); // UNKNOWN

  assert(x.a[0] == 7);

  struct kala xnn;
  for(int l=0; l < 5; l++) {
    xnn.a[l] = 42;
  }

  assert(xnn.a[3] == 42);




  struct kala xn;

  struct kala xs[5];
  // xs[0] = x;

  for(int j=0; j < 4; j++) {
    xs[j] = xn;
    for(int k=0; k < 5; k++) {
      xs[j].a[k] = 7;
    }
  }

  ip = &xs[3].a[0];

  int i = *ip;
  assert(*ip == 7);

  assert(xs[3].a[0] == 7);
**/

}

void ptrToArray() {
  int array1[10000000];
  int array2[10000000];
  
  int* ptr;

  if(rand()) {
    ptr = &array1;
    *ptr = 5;

    assert(*ptr == 5);  
  }
  else {
    ptr = &array2;
    *ptr = 5;

    assert(*ptr == 5);
  }

  // Since ptr could point to different arrays, the update here can not be precise
  *ptr = 6;

  assert(*ptr == 6); // UNKNOWN
}

void unionWeirdness() {
  // --------------- TEST 3 -------------------
  union uArray ua;
  int i3=0;

  (ua.b)[2] = 1;
  (ua.a)[2] = 1;

  while(i3< 5) {
    (ua.a)[i3] = 42;
    i3++;
  } 

  assert(ua.a[i3-1] == 42);

  // --------------- TEST 4 ----------------
  union uStruct us;
  int i4=0;

  us.b = 4;
  us.k.a[0] = 0; // (k, { a -> [0,0] }) is the result here, 
                // which is obviously unsound as it is not an array, leads to
                // errors trying to update an array when there is none
  //us.k.a[1] = 42;

  assert(us.k.a[3] == 0); // UNKOWN


  while(i4<5) {
    us.k.a[i4] = 42;
    i4++;
  }
}

void failing_example() {

}

void array_access_in_subscript() {
  int a[42];
  int i = 0;

  while(i < 42) {
    a[i] = 0;
    i++;
  }

  i = 0;

  a[a[0]] = 2;
}


int main () {
  // failing_example();
  // return 0;

  interesting();
  array_access_in_subscript();
  // unionWeirdness();
  // ptrToArray();
  // return 0;

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
  
  assert(a[((int)ip) % 2]==2); // strange, eh?
  
  // writing to unknown index:
  // NB! We assume the index is in bounds!
  if (k1) i=0; else i=1;
  a[i] = 0;
  assert(a[0] == 0); // UNKNOWN
  assert(a[1] == 0); // UNKNOWN

  // This assertion will fail depending on the used array domain
  // SimpleFragmented does know this
  // assert(a[2] == 0); // UNKNOWN 
  
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

  // This assertion will succeed depending on the used array domain
  // SimpleFragmented does know this
  // assert(*ip == 5); // UNKNOWN
  assert(c[1] == 5); // UNKNOWN

  // Now testing arrays inside structs.
  struct kala x;
  ip = x.a;
  x.a[0] = 7;
  *ip = 7;
  assert(*ip == 7);
  
  // (typeless) Top index
  // this assert was in here from the previous array domain, but actually asserts
  // sth that is unsound, therfore the UNKNOWN was added
  assert(x.a[top] == 7); // UNKNOWN

  assert(x.a[0] == 7);

  // And finally array of structs
  struct kala xs[5];
  xs[0] = x;
  ip = &xs[0].a[0];
  assert(*ip == 7);
  
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