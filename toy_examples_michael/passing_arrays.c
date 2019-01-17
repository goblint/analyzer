int main(void) {
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
  int y = array1[0];
}

int sth(int* ptr, int length) {
  int val = 0;
  for(int i=0; i<length;i++) {
    val = *ptr;
    ptr++;
  }

  return val;
}
