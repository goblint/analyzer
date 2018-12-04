int main(void) {
  int b, c;
  // b = 2;
  // c = b;

  // somefunc();
  does_sth_with_arrays();
  does_sth_else_with_arrays();
  return 0;
}

void does_sth_with_arrays() {
    int two = 2;


    int array[42];
    int i, e;

    i = 0;
    e = i;
    nothing();
    while(i < 42) {
      array[i] = 0;
      i++;
      e = i;
      nothing();
    }
}

void does_sth_else_with_arrays() {
    int array[42];
    int i, e, f;

    i = 1;
    e = i-1;
    nothing();
    while(i < 43) {
      array[i-1] = 0;
      i++;
      e = i-1;
      nothing();
    }
}

/** void does_sth_with_arrays_cil(void)
{
  int array[42];
  int i;
  int e;
  {
      {
        i = 0;
        e = i;
        nothing();
      }
      {
        while (1) {
          while_continue:  ;

          if (! (i < 42)) {
            goto while_break;
          }

          {
            array[i] = 0;
            i ++;
            e = i;
            nothing();
          }
        }
      while_break: ;
      }
      return;
  }
} */



void nothing() {}
