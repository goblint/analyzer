// PARAM: --set ana.activated[0][+] "'region'"  --set ana.activated[0][+] "'var_eq'"  --set ana.activated[0][+] "'symb_locks'"  --set kernel true --set nonstatic true --set exp.region-offsets true
#include<linux/module.h>
#include<linux/list.h>
#include<linux/mutex.h>

struct s {
  int datum;
  struct list_head list;
}; 

struct list_head slot[10];
struct mutex slots_mutex[10];

struct s *new(int x) {
  struct s *p = kmalloc(sizeof(struct s), GFP_KERNEL);
  p->datum = x;
  INIT_LIST_HEAD(&p->list);
  return p;
}

void t1(int i) {
  struct s *p; 
  
  p = new(1);
  mutex_lock(&slots_mutex[i]);
  list_add(&p->list, &slot[i]); 
  p = new(2);
  list_add(&p->list, &slot[i]); 
  mutex_unlock(&slots_mutex[i]);
}

void t2 (int j) {
  struct s *pos;
  int j;
  
  mutex_lock(&slots_mutex[j]);
  list_for_each_entry(pos, &slot[j+1], list) {
    pos->datum++; // RACE!
  }
  mutex_unlock(&slots_mutex[j]);
}
