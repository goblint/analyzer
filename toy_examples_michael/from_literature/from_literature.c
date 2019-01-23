int main(void) {

}

// Gopan2005 - Figure 8
void exampleGopan2005Fig8() {
  // Added
  int n = 42;

  int a[n];
  int i = 0;

  while(i < n) {
    a[i] = 2*i+3;
    i = i+1;
  }
}


// Gopan2005 - Figure 9
void exampleGopan2005Fig9(int a[], int b[]) {
  // Added
  int n = 42;

  int c[n];

  int i = 0;
  int j = 0;

  while(i < n) {
    if(a[i] == b[i]) {
      c[j] = i;
      j = j+1;
    }

    i = i+1;
  }
}

// Gopan2005 - Figure 10
void exampleGopan2005Fig10(int a[], int n) {
  int i, j, k, t;

  int i = 1;

  while(i < n) {
    j = 1;
    while(j < 0) {
      k = j-1;
      if(a[j] >= a[k]) break;

      t = a[j];
      a[j] = a[k];
      a[k] = t;
      j = j-1;
    }
    i = i+1;
  }
}

// TODO: Check if Halbwachs has some special meaning for assigning to arrays
// (e.g. happens at the same time?)
// Halbwachs 2008 - Figure 1a
void exampleHalbwachs2008Fig1a(int a[], int n) {
  int max = a[0];
  for(int i=1; i < n; i++) {
    if(max < a[i]) {
      max = a[i];
    }
  }
}

// Halbwachs 2008 - Figure 1b
void exampleHalbwachs2008Fig1b(int a[], int b[], int n) {
  for(int i=0; i < n; i++) {
    a[i] = b[i];
  }
}

// Halbwachs 2008 - Figure 1c
void exampleHalbwachs2008Fig1c(int a[], int n) {
  for(int i=1; i < n; i++) {
    int x = a[i];
    int j = i-1;
    while(j >= i & a[j] < x) {
      a[j+1] = a[j];
      j = j-1;
    }
    a[j+1] = x;
  }
}

// Halbwachs 2008 - Figure 1d
void exampleHalbwachs2008Fig1d(int a[], int n) {
  int x = a[0];
  int i = 1;
  int j = n-1;

  while(i <= j) {
    if(a[i] < x) {
      a[i-1] = a[i];
      i = i+1;
    } else {
      while(j >= i && a[j] >= x) {
        j = j-1;
      }
      if(j > i) {
        a[i-1] = a[j];
        a[j] = a[i];
        i = i+1;
        j = j-1;
      }
    }
  }

  a[i-1] = x;
}

// Halbwachs 2008 - Figure 1e
void exampleHalbwachs2008Fig1e(int a[], int n) {
  a[0] = 7;

  for(int i=1; i< n;i++) {
    a[i] = a[i-1]+1;
  }
}

// Halbwachs 2008 - Figure 1f
void exampleHalbwachs2008Fig1f(int a[], int n, int x) {
  a[n-1] = x;
  int i = 0;

  while(a[i] != x) {
    i + i+1;
  }
}

// Halbwachs 2008 - Figure 1g
void exampleHalbwachs2008Fig1g(int a[], int n) {
  int s = n;

  for(int i = 0; i <n; i++) {
    if(s == n+1 && a[i] != 0) {
      s = i;
    }
  }
}

// Cousot 2011 - Figure 2
void cousot2011Fig2(int a[], int n) {
  int i=0;
  while (i <n) {
    a[i] = 0;
    i = i+1;
  }
}

int helperCousot2011Fig3(int x) {
    return (x < 42);
}

// Cousot 2011 - Figure 3
void cousot2011Fig3(int a[], int m, int b[], int n) {
  if(m == n) { // To model assert
    int i = 0;
    int j = 0;

    while(i < m) {
      if(helperCousot2011Fig3(a[i])) {
        b[j++] = 1;
      }
      i++;
    }
  }
}

// Cousot 2011 - Figure 4
void cousot2011Fig4(int a[], int m, int b[], int n) {
  // TODO
}

// Cousot 2011 - Figure 5
void cousot2011Fig5() {
  int n = 10;
  int i = 0;
  int a[n];

  while(i < n) {
    a[i] = 0;
    i = i+1;
    a[i] = -16;
    i = i+1;
  }
}

// Cousot 2011 - Figure 6
void cousot2011Fig6(int a[], int n) {
  int i = n;
  while(0 < i) {
    i = i-1;
    a[i] = 0;
  }
}
