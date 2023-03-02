// PARAM: --set ana.malloc.unique_address_count 1

#include <stdlib.h>
#include <stdint.h>
#include <goblint.h>

struct s {
  int x;
  int y;
};

int main() {
  struct s *ptr = malloc(sizeof(struct s));
  int p;

  ptr->x = 0;
  ptr->y = 1;

  __goblint_check(ptr->x == 0);
  __goblint_check(ptr->y == 1);

  ptr->x = 1;
  ptr->y = 0;
  __goblint_check(ptr->x == 1);
  __goblint_check(ptr->y == 0);
}
