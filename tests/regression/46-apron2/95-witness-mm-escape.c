// CRAM PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.validate 95-witness-mm-escape.yml
#include <pthread.h>
#include <goblint.h>

int *b;
pthread_mutex_t e;

void main() {

    int g = 8;
    int a;
    if(a) {
        g = 10;
    }

    b = &g;

    pthread_mutex_lock(&e);
}
