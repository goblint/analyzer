// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --enable witness.graphml.enabled --set ana.specification 'CHECK( init(main()), LTL(G valid-memtrack) )' --set ana.activated[+] memLeak --set ana.path_sens[+] memLeak --set ana.malloc.unique_address_count 1
struct _twoIntsStruct {
   int intOne ;
   int intTwo ;
};

typedef struct _twoIntsStruct twoIntsStruct;

void printStructLine(twoIntsStruct const *structTwoIntsStruct)
{
  return;
}

twoIntsStruct *foo() {
  twoIntsStruct *data;
  int tmp_1;

  if (tmp_1 != 0) {
    twoIntsStruct *dataBuffer = malloc(800UL);
    data = dataBuffer;
  }
  else {

    twoIntsStruct *dataBuffer_0 = malloc(800UL);
    data = dataBuffer_0;
  }
  return data;
}

int main(int argc, char **argv)
{
  twoIntsStruct *data;
  data = foo();

  printStructLine((twoIntsStruct const *)data);
  free((void *)data);

  return;
}
