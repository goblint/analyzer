// PARAM: --enable ana.sv-comp.functions
// Adapted from: https://github.com/sosy-lab/sv-benchmarks/blob/master/c/ldv-regression/test27-2.c
extern int __VERIFIER_nondet_int(void);

struct dummy {
  int *array;
};

struct cont {
  struct dummy **array;
};

int check(struct cont *pc, int i)
{
   return pc->array[i]->array[i] == i;
}

int main()
{
  struct cont cont;
  struct dummy dummy;
  struct dummy *dummies[10];
  int a[10];
  int i, *pa;
  i = __VERIFIER_nondet_int();
  for (int j = 0; j < 10; j++) {
      a[j] = __VERIFIER_nondet_int();
      // this task is not memory safe as the access in lin 38 is off by one
      // compared to the initialized element of line 36, thus we might as well
      // use NULL here
      dummies[j] = 0;
  }
  if (i >= 0 && i < 9) {
    a[i] = i;
    dummy.array = &a[i];
    dummies[i + 1] = &dummy;
    cont.array = &dummies[0];
    pa = &cont.array[i]->array[i];
    if (a[i] > 0) {
      i =  dummies[i + 1]->array[i] - 10;
      while (i < *pa) {
        ++i;
      }
      if (!check(&cont, i)) {
        goto ERROR;
      }
    }
  }

  return 0;

  ERROR: {reach_error();abort();}
  return 1;
}
