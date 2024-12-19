// CRAM PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable warn.deterministic --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.validate 95-witness-mm-escape.yml
#include<pthread.h>
int *b;
pthread_mutex_t e;

void* other(void* arg) {
    pthread_mutex_lock(&e);
    *b = -100;
    pthread_mutex_unlock(&e);

    return NULL;
}

void main() {
    pthread_t t;
    pthread_create(&t, NULL, other, NULL);
    int g = 8;

    b = &g;

    pthread_mutex_lock(&e);
}
