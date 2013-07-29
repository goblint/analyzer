// SKIP PARAM: --set kernel true --set mainfun[+] "'test_init'" --set dbg.debug true --set exp.list-type true

#include <linux/mutex.h>
#include <linux/list.h>

struct elem {
  struct list_head item;
  int              d;
};

struct elem e1,e2;

LIST_HEAD(my_list);

int sum(){
  struct elem *e;
  int sum = 0;

  list_for_each_entry(e,&my_list,item) { //NOWARN
    sum = e->d;
  e->d = 0; //NOWARN!!1
  }
  return sum;
}


int __init test_init(void) {
  int t;
  list_add(&e1.item, &my_list); //NOWARN
  list_add(&e2.item, &my_list); //NOWARN

  list_del(&e2.item);

  t = sum();

  assert(t == 0); //NOWARN
  assert(t != 0); //FAIL

  return t;
}

 
