// PARAM: --set ana.activated[0][+] "'var_eq'"  --set ana.activated[0][+] "'symb_locks'"  --set ana.activated[0][+] "'region'"  --set exp.region-offsets true
#include<pthread.h>

struct list_head {
  struct list_head *next ;
  struct list_head *prev ;
};

struct s {
  int datum ;
  struct list_head list ;
};

struct cache {
  struct list_head slot[10] ;
  pthread_mutex_t slots_mutex[10] ;
};

struct cache c  ;

inline static struct list_head *lookup (int d) {
  int hvalue;
  struct list_head *p;
  p = c.slot[hvalue].next;
  return p;
}

void *f(void *arg) { 
  struct s *pos ;
  int j;
  struct list_head  const  *p ;
  struct list_head  const  *q ;

  while (j < 10) {
    pthread_mutex_lock(&c.slots_mutex[j]);
    p = c.slot[j].next;
    pos = (struct s *)((char *)p - (unsigned int )(& ((struct s *)0)->list));

    while (& pos->list != & c.slot[j]) {
      pos->datum++; // RACE!
      q = pos->list.next;
      pos = (struct s *)((char *)q - (unsigned int )(& ((struct s *)0)->list));
    }

    pthread_mutex_unlock(&c.slots_mutex[j]);
    j ++;
  }
  return 0;
}

int main() {
  struct list_head *p1, *p2;
  pthread_t t;
  p1 = lookup(1);
  p2 = lookup(2);
  p1->next = p2->next;
  // per-element scheme no longer safe.
  pthread_create(&t, NULL, f, NULL);
  return 0;
}
