// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --enable ana.apron.threshold_widening --sets ana.apron.privatization mutex-meet-tid

#include <pthread.h>
#include <assert.h>

int pendingIo, stoppingEvent, stopped, status;
int conjunct_flag;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void* T1_QW2004(void* arg) {
    int pending;
    pthread_mutex_lock(&m);
    pendingIo--;
    pending = pendingIo;
    if (pending == 0) {
        stoppingEvent = 1;
        stopped = 0;
    }
    else
        stopped--;

    assert(pendingIo == stopped);
    pthread_mutex_unlock(&m);
    return NULL;
}

int main() {
    int status, pending;
    pendingIo = 1;
    stoppingEvent = 0;
    stopped = 1;

    status = 0;

    pthread_t t1;
    pthread_create(&t1, 0, T1_QW2004, 0);

    pthread_mutex_lock(&m);
        if (stopped==0)
            status = -1;
        else {
            pendingIo++;
            stopped++;
            status = 0;
        }
        assert(pendingIo == stopped);
    pthread_mutex_unlock(&m);

    pthread_mutex_lock(&m);
    if (status == 0)
        assert(pendingIo == stopped);
    pthread_mutex_unlock(&m);

    pthread_mutex_lock(&m);
    if (pendingIo > 0) {
        pendingIo--;
        stopped--;
    }
    pending = pendingIo;
    if (pending == 0)
        stoppingEvent = 1;

    assert(pendingIo == stopped);
    pthread_mutex_unlock(&m);
    return 0;
}
