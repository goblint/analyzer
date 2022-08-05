#include<stdio.h>
#include<assert.h>

struct kala {
  int x;
};

struct node {
  int data;
  struct node *next;
};

extern void update_list(struct node *x);

void invalid (int **x) {
  int k;
  **x = k;
  return;
}

int main () {
  int i = 0;
  int *ip;
  struct kala k;

  struct node n1;
  struct node n2;

  // Testing invalidation of nested ptrs
  i = 7; k.x =13;
  ip = &i;
  invalid(&ip);
  __goblint_check(i == 7); // UNKNOWN
  *ip = 3;
  __goblint_check(i == 3);

  ip = &k.x;
  invalid(&ip);
  __goblint_check(k.x == 13); // UNKNOWN
  *ip = 5;
  __goblint_check(k.x == 5);

  // Testing invalidation of linked structs
  n1.data = 1;
  n2.data = 2;
  n1.next = &n2;
  n2.next = &n1;
  __goblint_check(n1.next->data == 2);
  __goblint_check(n2.next->data == 1);

  update_list(&n1);
  __goblint_check(n1.data == 1); // UNKNOWN
  __goblint_check(n2.data == 2); // UNKNOWN

  return 0;
}

