// PARAM: --set ana.activated[0][+] "'region'"  --set kernel true --set nonstatic true 
#include<linux/module.h>
#include<linux/list.h>
#include<linux/mutex.h>

struct s {
  struct list_head list;
}; 

struct list_head A, B;

static DEFINE_MUTEX(A_mutex);
static DEFINE_MUTEX(B_mutex);


void t1() {
  struct s *p = kmalloc(sizeof(struct s), GFP_KERNEL);
  INIT_LIST_HEAD(&p->list);
  
  mutex_lock(&A_mutex);
  list_add(&p->list, &A); 
  if (p->list.next) 
    p->list.next->prev = NULL; //RACE
  mutex_unlock(&A_mutex);
}

void t2 () {
  struct s *p = kmalloc(sizeof(struct s), GFP_KERNEL);
  INIT_LIST_HEAD(&p->list);
  
  mutex_lock(&B_mutex);
  list_add(&p->list, &A);
  mutex_unlock(&B_mutex);
}
