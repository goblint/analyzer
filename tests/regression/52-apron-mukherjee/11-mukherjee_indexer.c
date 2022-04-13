// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins

#include <pthread.h>
#include <assert.h>

int MAX, SIZE, array;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* W1_Indexer(void* arg){
    int m = 0;
    int w = 0;
    int h = 0, rv = 0;

    while(m < MAX) {
        w = (++m) * 12;
        h = (w*7) % SIZE;

        assert(h>=0);

        rv = 0;
        h = rv + 1;

        while(rv == 0) {
            pthread_mutex_lock(&lock);
            if(array == 0) {
                array = h;
                rv = 1;
            }
            else
                h = (h + 1) % SIZE;
            pthread_mutex_unlock(&lock);
        }
    }
    return 0;
}

void* W2_Indexer(void* arg){
    int m = 0;
    int w = 0;
    int h = 0, rv = 0;

    while(m < MAX) {
        w = (++m) * 12;
        h = (w*7) % SIZE;

        assert(h>=0);

        rv = 0;
        h = rv + 1;

        while(rv == 0) {
            pthread_mutex_lock(&lock);
            if(array == 0) {
                array = h;
                rv = 1;
            }
            else
                h = (h + 1) % SIZE;
            pthread_mutex_unlock(&lock);
        }
    }
    return 0;
}

int main() {
    SIZE = 128;
    MAX = 4;
    array = 0;
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, W1_Indexer, 0);
    pthread_create(&t2, 0, W2_Indexer, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);

    return 0;
}
