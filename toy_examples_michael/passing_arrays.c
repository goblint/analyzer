int main(void) {
  int* array = arrayOf42();
  int val = array[10];

  int array1[1];
  int array2[1];

  array1[0] = 42;
  array2[0] = 42;

  // getFirst(array1);
  // getFirst(array2);
  getFirst(array);
}

int* arrayOf42() {
  int* array = (int *)malloc(sizeof(int)*20);

  for(int i=0; i < 5; i++) {
    array[i] = 42;
  }

  for(int i=1; i < 5; i++) {
    array[i] = 43;
  }

  return array;
}

void getFirst(int arg[]) {
  int x = arg[0];
}
