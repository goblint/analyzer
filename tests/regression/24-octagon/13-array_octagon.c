// SKIP PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.activated[+] apron --set sem.int.signed_overflow assume_none
#include <goblint.h>

void main(void) {
  example0();
  example1();
  example2();
  example3();
  example4();
  example4a();
  example4b();
  example4c();
  example5();
  example6();
  example7();
  example8();
  mineEx1();
}

void example0(void) {
  int a[20];
  int i = 0;
  int j = 0;
  int top;
  int z;

  // Necessary so we can not answer the queries below from the base domain
  // and actually test the behavior of the octagons
  int between1and8;
  if(between1and8 < 1) {
    between1and8 = 1;
  }

  if(between1and8 > 8) {
    between1and8 = 8;
  }

  while(i < 20) {
    a[i] = 0;
    i++;
  }

  while(j < between1and8) {
    a[j] = 1;
    j++;
  }

  a[j] = 2; // a -> (j,([1,1],[2,2],[0,0]))


  z = j;

  // Values that may be read are 1 or 2
  __goblint_check(a[z] == 1); // FAIL
  __goblint_check(a[z] == 2);
  __goblint_check(z >= 0);
  __goblint_check(z <= j);
  __goblint_check(a[z] == 0); //FAIL
}

void example1(void) {
  int a[20];
  int i = 0;
  int j = 0;
  int top;
  int z;

  // Necessary so we can not answer the queries below from the base domain
  // and actually test the behavior of the octagons
  int between1and8;
  if(between1and8 < 1) {
    between1and8 = 1;
  }

  if(between1and8 > 8) {
    between1and8 = 8;
  }

  while(i < 20) {
    a[i] = 0;
    i++;
  }

  while(j < between1and8) {
    a[j] = 1;
    j++;
  }

  a[j] = 2; // a -> (j,([1,1],[2,2],[0,0]))

  if(top) {
    z = j;
  } else {
    z = j-1;
  }

  // Values that may be read are 1 or 2
  __goblint_check(a[z] == 1); //UNKNOWN
  __goblint_check(a[z] == 2); //UNKNOWN
  __goblint_check(z >= 0);

  // Relies on option sem.int.signed_overflow assume_none
  __goblint_check(z <= j);
  __goblint_check(a[z] != 0);
}

void example2(void) {
  int a[20];
  int i = 0;
  int j = 0;
  int top;
  int z;

  // Necessary so we can not answer the queries below from the base domain
  // and actually test the behavior of the octagons
  int between1and8;
  if(between1and8 < 1) {
    between1and8 = 1;
  }

  if(between1and8 > 8) {
    between1and8 = 8;
  }

  while(i < 20) {
    a[i] = 0;
    i++;
  }

  while(j < between1and8) {
    a[j] = 2;
    j++;
  }

  a[j] = 1; // a -> (j,([2,2],[1,1],[0,0]))

  if(top) {
    z = j;
  } else {
    z = j+1;
  }

  // Values that may be read are 1 or 0
  __goblint_check(a[z] == 1); //UNKNOWN
  __goblint_check(a[z] == 0); //UNKNOWN

  // Relies on option sem.int.signed_overflow assume_none
  __goblint_check(a[z] != 2);
}

// Simple example (employing MustBeEqual)
void example3(void) {
  int a[42];
  int i = 0;
  int x;

  while(i < 42) {
    a[i] = 0;
    int v = i;
    x = a[v];
    __goblint_check(x == 0);
    i++;
  }
}

// Simple example (employing MayBeEqual / MayBeSmaller)
void example4(void) {
  int a[42];
  int i = 0;

  while(i<=9) {
    a[i] = 9;
    int j = i+5;
    a[j] = 42;

    // Here we know a[i] is 9 when we have MayBeEqual
    __goblint_check(a[i] == 9);

    // but only about the part to the left of i if we also have MayBeSmaller
    if(i>0) {
      int k = a[i-1];
      __goblint_check(k == 9);

      int l = a[0];
      __goblint_check(l == 9);
    }

    i++;
  }
}
// Just like the example before except that it tests correct behavior when variable order is reversed
void example4a(void) {
  int a[42];
  int j;
  int i = 0;

  while(i<=9) {
    a[i] = 9;
    j = i+5;
    a[j] = 42;

    // Here we know a[i] is 9 when we have MayBeEqual
    __goblint_check(a[i] == 9);

    // but only about the part to the left of i if we also have MayBeSmaller
    if(i>0) {
      __goblint_check(a[i-1] == 9);
    }

    i++;
  }
}

// Just like the example before except that it tests correct behavior when operands for + are reversed
void example4b(void) {
  int a[42];
  int j;
  int i = 0;

  while(i<=9) {
    a[i] = 9;
    j = 5+i;
    a[j] = 42;

    // Here we know a[i] is 9 when we have MayBeEqual
    __goblint_check(a[i] == 9);

    // but only about the part to the left of i if we also have MayBeSmaller
    if(i>0) {
      __goblint_check(a[i-1] == 9);
    }

    i++;
  }
}

// Like example before but iterating backwards
void example4c(void) {
  int a[42];
  int j;
  int i = 41;

  while(i > 8) {
    a[i] = 7;
    a[i-2] = 31;

    if(i < 41) {
      __goblint_check(a[i+1] == 7);
    }

    i--;
  }
}

void example5(void) {
  int a[40];
  int i = 0;

  // This is a dirty cheat to get the array to be partitioned before entering the loop
  // This is needed because the may be less of the octagons is not sophisticated enough yet.
  // Once that is fixed this will also work without this line
  a[i] = 0;

  while(i < 42) {
    int j = i;
    a[j] = 0;
    i++;

    __goblint_check(a[i] == 0); //UNKNOWN

    __goblint_check(a[i-1] == 0);
    __goblint_check(a[j] == 0);

    if (i>1) {
      __goblint_check(a[i-2] == 0);
      __goblint_check(a[j-1] == 0);
    }
  }
}

void example6(void) {
  int a[42];
  int i = 0;
  int top;

  while(i<30) {
    a[i] = 0;
    i++;

    __goblint_check(a[top] == 0); //UNKNOWN

    int j=0;
    while(j<i) {
      __goblint_check(a[j] == 0);
      j++;
    }
  }
}

void example7(void) {
  int top;

  int a[42];
  int i = 0;

  int j;

  while(i<30) {
    a[i] = 0;
    i++;

    if(i > 10) {
      if(top) {
        j = i-5;
      } else {
        j = i-7;
      }

      __goblint_check(a[j] == 0);
    }
  }
}

void example8(void) {
  int a[42];
  int i = 0;
  int j = i;

  int N;

  if(N < 5) {
    N = 5;
  }
  if(N > 40) {
    N = 40;
  }


  while(i < N) {
    a[i] = 0;
    i++;
    j = i;
    a[j-1] = 0;
    a[j] = 0;
    j++;      // Octagon knows -1 <= i-j <= -1
    i = j;    // Without octagons, we lose partitioning here because we don't know how far the move has been

    __goblint_check(a[i-1] == 0);
    __goblint_check(a[i-2] == 0);
  }

  j = 0;
  while(j < N) {
    __goblint_check(a[j] == 0);
    j++;
  }
}

// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf
void mineEx1(void) {
  int X = 0;
  int N = rand();
  if(N < 0) { N = 0; }

  while(X < N) {
    X++;
  }

  __goblint_check(X-N == 0);
  // __goblint_check(X == N); // Currently not able to assert this because octagon doesn't handle it

  if(X == N) {
    N = 8;
  } else {
    // is dead code but if that is detected or not depends on what we do in branch
    // currenlty we can't detect this
    N = 42;
  }
}
