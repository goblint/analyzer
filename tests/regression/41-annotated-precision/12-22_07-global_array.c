// PARAM: --set solver td3 --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper']" --set exp.privatization none --disable ana.int.def_exc --enable exp.annotated.precision --set ana.int.refinement fixpoint
int global_array[50];

int main(void) __attribute__((precision("interval"))) {
  some_func();

  int x = global_array[5];
  assert(x == 0); //UNKNOWN
  assert(x == 42); //UNKNOWN
}


void some_func(void) __attribute__((precision("interval"))) {
  global_array[0] = 5;

  for(int i=1; i < 50; i++) {
    global_array[i] = 42;
  }

  int x = global_array[0];
  assert(x == 42); //FAIL
}
