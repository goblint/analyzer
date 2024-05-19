#include <pthread.h>
struct s {
  int occupied;
  int closed;
};

struct s pqb;
pthread_mutex_t mtx;

void pqueue_close() {
  pthread_mutex_lock(&mtx);
  pqb.closed = 1;
  pthread_mutex_unlock(&mtx);
}

void* thread(void* arg) {
  pthread_mutex_lock(&mtx);

  if(pqb.occupied < 2) {
    pqb.occupied++;
  }
  
  pthread_mutex_unlock(&mtx);
 }

int main() {
  pthread_t worker;

  pthread_create(&worker, 0, &thread, 0);
  pqueue_close();
  return 0;
}


// rm out.txt && ./goblint --conf conf/traces.json --set ana.base.privatization protection 2.c --enable allglobs --enable dbg.timing.enabled --enable warn.debug -v --sets exp.priv-prec-dump level-ip.protection.prec --enable ana.int.interval && ./goblint --conf conf/traces.json --set ana.base.privatization write 2.c --enable allglobs --enable dbg.timing.enabled --enable warn.debug -v --sets exp.priv-prec-dump level-ip.write.prec --enable ana.int.interval &&  ./privPrecCompare level-ip.protection.prec level-ip.write.prec &> out.txt
