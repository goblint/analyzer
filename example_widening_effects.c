#include <pthread.h>
int occupied;

pthread_mutex_t mtx;


void* thread(void* arg) {
  pthread_mutex_lock(&mtx);

  if(occupied < 2) {
    occupied++;
  }

  pthread_mutex_unlock(&mtx);
 }

int main() {
  pthread_t worker;

  pthread_create(&worker, 0, &thread, 0);

  pthread_mutex_lock(&mtx);
  occupied = 0;
  pthread_mutex_unlock(&mtx);
  
  return 0;
}


// rm out.txt && ./goblint --conf conf/traces.json --set ana.base.privatization protection 2.c --enable allglobs --enable dbg.timing.enabled --enable warn.debug -v --sets exp.priv-prec-dump level-ip.protection.prec --enable ana.int.interval && ./goblint --conf conf/traces.json --set ana.base.privatization write 2.c --enable allglobs --enable dbg.timing.enabled --enable warn.debug -v --sets exp.priv-prec-dump level-ip.write.prec --enable ana.int.interval &&  ./privPrecCompare level-ip.protection.prec level-ip.write.prec &> out.txt
