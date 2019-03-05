int main(void) {
  int top;

  int a[20];
  int b[20];
  int i = 0;
  int y;

  if(top) {
    while(i < 20) {
      a[i] = 42;
      i++;
      y = 7;
    }
  } else {
    while(i < 20) {
      b[i] = 42;
      i++;
    }
  }

  int x = a[5];
  small();
  two();
}

int two() {
  int i;
  int top;

  int a[20];

  if(top) {
    i = 3;
    a[i] = 0;
  } else {
    i = 7;
    a[i] = 7;
  }

  int x = a[i];
}

int small() {
  int a[20];
  int top;

  if(top) {
    a[0] = 5;
  }

  int x = a[0];
}