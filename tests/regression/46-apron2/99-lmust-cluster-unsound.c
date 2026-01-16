// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12
#include<pthread.h>
#include <goblint.h>

int a;
int b;

pthread_mutex_t f;

void nothing() {}
void nothing2() {
    pthread_mutex_lock(&f);
    a = 5;
    b = 5;
    pthread_mutex_unlock(&f);
}


void main() {
    pthread_t tid;
    int x;
    pthread_create(&tid, 0, &nothing, NULL);

    pthread_mutex_lock(&f);
    b = 5;
    pthread_mutex_unlock(&f);

    pthread_t tid2;
    pthread_create(&tid2, 0, &nothing2, NULL);

    pthread_mutex_lock(&f);
    x = a;
    pthread_mutex_unlock(&f);

    __goblint_check(x == 5); //UNKNOWN!
}
