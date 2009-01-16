// PARAM: --kernel --allfuns --regions
#include<linux/module.h>
#include<linux/list.h>
#include<linux/mutex.h>

struct s {
  int datum;
  struct list_head list;
}; 

struct list_head A, B;

static DEFINE_MUTEX(A_mutex);
static DEFINE_MUTEX(B_mutex);


void t1() {
  struct s *p = kmalloc(sizeof(struct s), GFP_KERNEL);
  p->datum = 7;
  INIT_LIST_HEAD(&p->list);
  
  mutex_lock(&A_mutex);
  list_add(p, &A); 
  p->datum++; // RACE!
  mutex_unlock(&A_mutex);
}

void t2 () {
  struct s *p = kmalloc(sizeof(struct s), GFP_KERNEL);
  p->datum = 9;
  INIT_LIST_HEAD(&p->list);
  
  mutex_lock(&B_mutex);
  list_add(p, &A);
  p->datum++; // RACE!
  mutex_unlock(&B_mutex);
}
