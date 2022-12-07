#include <stddef.h>
#include <goblint.h>

struct node {
  struct node *left;
  struct node *right;
  int key;
};

// https://old.reddit.com/r/programminghorror/comments/jgrpcu/on_sedgewicks_original_presentation_for_llrb_trees/
struct node* min(struct node *root) {
  struct node *x = root;
  while (x != NULL)
    x = x->left;
  if (x == NULL) // WARN (dead branch)
    return NULL;
  else
    return x;
}

int main() {
  struct node *root;
  struct node *m = min(root);
  __goblint_check(m == NULL);
  return 0;
}
