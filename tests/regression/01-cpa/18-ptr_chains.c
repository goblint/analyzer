#include<stdio.h>
#include<assert.h>
void assert_unknown(int x) { };

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
  assert_unknown(i);
  *ip = 3;
  assert(i == 3);
  
  ip = &k.x;
  invalid(&ip);
  assert_unknown(k.x);
  *ip = 5;
  assert(k.x == 5);

  // Testing invalidation of linked structs
  n1.data = 1;
  n2.data = 2;
  n1.next = &n2;
  n2.next = &n1;
  assert(n1.next->data == 2);
  assert(n2.next->data == 1);

  update_list(&n1);
  assert_unknown(n1.data);
  assert_unknown(n2.data);

  return 0;
}

