// Handcrafted examples I used when designing the domain
int main(void) {
  example1();
  example2();
  example3();
  example4();
  example5();
  example6();
  example7();
  example8();
  example9();
  example10();
}

// Simple example
void example1(void) {
  int a[42];
  int i = 0;

  while(i < 42) {
    a[i] = 0;
    i++;
  }
}

// More complicated logic in initialization
void example2(void) {
  int a[42];
  int i = 0;

  while(i < 42) {
    a[i] = 0;

    if(i < 7) {
      a[i] = 3;
    }

    i++;
  }
}

// More complicated expression to index rather than just a variable
void example3(void) {
  int a[42];
  int i = 1;

  while(i < 43) {
    a[i-1] = 0;
    i++;
  }
}

// What if the content domain requires widening?
void example4(void) {
  int a[42];
  int i = 0;

  while(i < 42) {
    a[i] = i;
    i++;
  }
}

// Example showing that partial initialization is problematic when we start from
// the middle (not from the very left/right)
void example5(void) {
  int a[42];
  int i = 1;

  while(i < 42) {
    a[i] = 0;
    i++;
  }
}

// Two values initialized in one loop
void example6(void) {
  int a[42];
  int i = 0;

  while(i < 42) {
      a[i] = 0;
      i++;
      a[i] = 1;
      i++;
  }
}

// What happens when we assign a variable in the expression e so that we lose
// it
void example7(void) {
  int a[42];
  int i = 0;
  int j;

  while(i < 42) {
    a[i] = 0;
    i++;
    j = i;
    i = 31;
    i = j;
  }
}

// What happens when we assign a variable in the expression e so that we lose
// it, but still have some remaining information after the first iteration
void example8(void) {
  int a[42];
  int i = 41;
  int j;

  while(i >= 0)  {
    a[i] = 0;
    i--;
    j = i;
    i = 41;
    i = j;
  }
}

// Nested loops
void example9(void) {
  int a[42];
  int i = 0;
  int j;

  while(i < 42) {
    a[i] = 0;
    j = i;

    while(j < 42) {
      a[j] = 2;
      j++;
    }

    i++;
  }
}

// What if two different vars are used to index in the array?
void example10(void) {
  int a[42];

  int x = srand();

  if(x == 0) {
    int j = 0;

    while(j < 42) {
      a[j] = 0;
      j++;
    }
  } else {
    int i = 0;

    while(i < 42) {
      a[i] = 0;
      i++;
    }
  }
}