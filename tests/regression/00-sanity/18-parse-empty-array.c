// Test for issue in https://github.com/goblint/cil/issues/19
// NOCRASH
static int a[];
static int a[] = {};
static int b[0] = {};

struct blub {
  int member;
  int m2[];
};

struct blub one = {0,{1}};
struct blub two = {0,{0,0}};
struct blub three = {};
struct blub four;
struct blub five = {-3, {}};

void main(void) {
  int a[] = {};
}
