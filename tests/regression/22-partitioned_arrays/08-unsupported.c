// PARAM: --enable ana.int.interval --disable exp.fast_global_inits --set ana.base.arrays.domain partitioned

// This is just to test that the analysis does not cause problems for features that are not explicitly dealt with
int main(void) {
  callok();
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
