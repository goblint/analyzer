  $ ./transform.sh remove_dead_code -- 03-deadcode-globals.c | clang-format
  struct struct1 {
    int field1;
    char field2;
  };
  typedef struct struct1 struct1_named1;
  typedef struct struct1 struct1_named3;
  typedef struct struct1 struct1_named4;
  struct struct1 const struct1_value1 = {(int const)0, (char const)'a'};
  struct1_named3 const struct_f(void) {
  
    { return (struct1_value1); }
  }
  int struct_pointer_f(struct1_named4 *x) {
  
    { return ((int)(x->field1 + 1)); }
  }
  int const global1 = (int const)1;
  int const global2 = global1 + 7;
  int main(void) {
    struct1_named1 x;
    struct1_named3 tmp;
    int y;
  
    {
      {
        tmp = (struct1_named3)struct_f();
        x = tmp;
        y = (int)global2;
        struct_pointer_f(&x);
      }
      return (0);
    }
  }
