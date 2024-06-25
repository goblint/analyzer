// PARAM: --enable ana.int.interval --enable solvers.td3.narrow-sides.enabled
#include <stdlib.h>

int state = 0;

void *thread(void *) {
    while (1) {
        int input;
        int next_state;
        int current_state = state;
        switch (current_state) {
            case 0:
                if(input)
                    next_state = current_state + 1;
                else
                    next_state = current_state;
                break;
            case 1:
                if(input)
                    next_state = current_state + 1;
                else
                    next_state = current_state + 2;
                break;
            case 2:
                next_state = 0;
                break;
            case 3:
                next_state = 4;
                break;
            case 4:
                next_state = 5;
                break;
            case 5:
                next_state = 5;
                break;
            default:
                next_state = -1;
        }
        state = next_state;
    }
}

int main(void) {
    int id;
    pthread_create(&id, NULL, thread, NULL);
    __goblint_check(state >= -1);
    __goblint_check(state <= 5);
    pthread_join(&id, NULL);
}
