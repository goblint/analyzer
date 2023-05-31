// SKIP: this is an input file for cram tests

// structs

struct struct1 {
  const int field1;
  const char field2;
};

typedef struct struct1 struct1_named1;  // used, thus kept
typedef struct struct1 struct1_named2;  // unused, removed
typedef struct struct1 struct1_named3;  // only used as return type, kept
typedef struct struct1 struct1_named4;  // only used as an argument type, kept

struct struct2 {
  int field3;
  struct1_named1 *field4;  // use of struct1_named1 and struct struct1
};

const struct struct1 struct1_value1 = {
  .field1 = 0,
  .field2 = 'a'
};

struct1_named3 const struct_f() {
  return struct1_value1;
}

int struct_pointer_f(struct1_named4 *x) {
  return x->field1 + 1;
}

// globals referencing each other

const int global1 = 1;  // referenced (indirectly)
const int global2 = global1 + 7;  // referenced (directly in main)
const int global3 = 2;  // referenced by global4 only
const int global4 = global3 + 9;

// reference each other, but otherwise unreferenced
int mutually_recursive2(void);
int mutually_recursive1() { return mutually_recursive2(); }
int mutually_recursive2() { return mutually_recursive1(); }


int main() {
  struct1_named1 x = struct_f();
  int y = global2;
  struct_pointer_f(&x);
}
