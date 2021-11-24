// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set sem.int.signed_overflow assume_none

#include <pthread.h>
#include <assert.h>

volatile int len;
volatile int next;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* thr1(void* arg) {
    int c, end, temp;
    c = 0;
    end = 0;
    pthread_mutex_lock(&lock);
    temp = len;

    if(next + 10 <= len) {
        c = next;
        end = next + 10;
        next = next + 10;
    }
    pthread_mutex_unlock(&lock);

    while(c < end) {
        assert(c >= 0);
        assert(c <= temp);
        c = c +1;
    }
    return NULL;
}

void* thr2(void* arg) {
    int c, end, temp;
    c = 0;
    end = 0;
    pthread_mutex_lock(&lock);
    temp = len;

    if(next + 10 <= len) {
        c = next;
        end = next + 10;
        next = next + 10;
    }
    pthread_mutex_unlock(&lock);

    while(c < end) {
        assert(c >= 0);
        assert(c <= temp);
        c = c +1;
    }
    return NULL;
}

int main(int argc, char* argv[]) {
    pthread_t t1;
    pthread_t t2;
    next = 0;
    int t;
    len = t;
    if (len > 0) {
	    pthread_create(&t1, 0, thr1, 0);
	    pthread_create(&t2, 0, thr2, 0);
        pthread_join(t1, 0);
        pthread_join(t2, 0);
    }
    return 0;
}
