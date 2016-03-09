// PARAM: --set ana.activated[+] "'symb_locks'"  --set ana.activated[+] "'var_eq'" 
#include <stdlib.h>

typedef long long pthread_t;
typedef long long pthread_mutex_t;

// the lock label
#pragma existential(t,"t.lock","&t.data")
// the address of the field
#pragma existential(t,"t.data")

struct t {
  pthread_mutex_t lock;
  int* data;
};

#define pack(v) (v)
#define start_unpack(v) 
#define end_unpack(v) 

void* f(struct t* in) {
  start_unpack(in);
  pthread_mutex_lock(&(in->lock));
  in->data++; // NOWARN
  pthread_mutex_unlock(&(in->lock));
  end_unpack(in);
  return 0;
}


int main() {
  int random;
  struct t __attribute__((packed)) *s = 0;
  struct t *p = 0;
  pthread_t t1, t2;

  if (random <= 0) random = 1 ;
  while(random > 0) {
    s = (struct t __attribute((packed)) *) malloc(sizeof (struct t));
    pthread_mutex_init(&s->lock, 0);
    p = pack(s);
    random--;
  }
  
  if (p == 0) exit(-1);
  
  pthread_create(&t1, 0, f, p);
  pthread_create(&t2, 0, f, p);
  return 1;
}
