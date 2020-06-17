// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --sets exp.partition-arrays.keep-expr "last" --set ana.activated "['base','expRelation']"
// This is just to test that the analysis does not cause problems for features that are not explicetly dealt with
int main(void) {
  vla();
  callok();
}

void vla(void) {
  // This is an example for VLAs, CIL turns them into alloca so array domain is not
  // used here.
  int top;
  int l = 5;

  if(top) {
    l = 6;
  }

  int a[l];

  for(int i=0; i < l-1; i++) {
    a[i] = 42;
  }
}

struct blub
{
    int blubby;
};

struct ms
{
    struct blub b;
    int x;
    int y;
    int z;
};


void fun(int *ptr) {
    *ptr++;
}

void callok(void) {
  struct ms *ptr  = kzalloc(sizeof(struct ms));

  fun(&ptr->b.blubby);

  ptr->b.blubby = 8;

  ptr->x = 47;
  ptr->y = 11;

  return 0;
}
