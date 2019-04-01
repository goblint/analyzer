int main(void) {
  simple();
  simple2();
  simple3();
  exampleEnter();
/**
  int i = 0;
  int a;
  int array1[10000000];

  int* ptr = &array1;
  *ptr = 5;

  int val = *ptr;
  ptr++;

  int array2[10];
  array2[9] = 42;

  a = 5;
  a = 7;

  while(i < 10000000) {
    array1[i] = 42;
    i++;
    a=2;
  }

  int* ptr2 = &(array1[2]);
  ptr2++;

  int z = sth(ptr2,10);

  int j = 0;
  while(j < 8) {
    ptr2++;
    j++;
  }

  int x = *ptr2;

  a = 5;

  int x = array1[i] + 7;
  int y = array1[0]; **/
}

void exampleEnter(void) {
  int arr[20];

  for(int i = 0; i < 20; i++)
  {
    arr[i] = 42;
    callee(arr);
  }
}

void callee(int* arr) {
  arr[0] = 7;
}

void simple3(void) {
  int array1[5];
  int *ptr = &array1;

  for(int i =0; i <5; i++) {
    *ptr = 42;
    ptr++;
  }

  int y = 7;
}

void simple2(void) {
  int array1[5];
  int *ptr = &(array1[4]);

  *ptr = 42;
  ptr--;
  *ptr=41;
  ptr--;
  *ptr=40;
}

void simple(void) {
  int array1[100];
  int* ptr = &array1;
  
  *ptr = 5;
  int v = *ptr;

  ptr++;
  *ptr = 6;
  ptr++;
  *ptr = 7;

  int j = &array1-ptr;
  int k = ptr-&array1;
  int l = ptr-array1;
  int m = array1-ptr;

  int* np = &array1;
  np++;
  np++;
  int x = *np;
}

int sth(int* ptr, int length) {
  int val = 0;
  for(int i=0; i<length;i++) {
    val = *ptr;
    ptr++;
  }

  return val;
}
