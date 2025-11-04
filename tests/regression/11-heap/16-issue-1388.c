// PARAM: --set ana.malloc.unique_address_count 2 --set ana.activated[-] threadid

typedef struct list {
 struct list *next;
} mlist;

mlist *head;

int insert_list(mlist *l){
  l = (mlist*)malloc(sizeof(mlist));
  l->next = head;

  head = l;
  return 0;
}

int main(void){
  mlist *mylist;
  insert_list(mylist);
  insert_list(mylist);

  while(head!=((void *)0)) {
    head = head->next;
  }

  __goblint_check(1);
}
