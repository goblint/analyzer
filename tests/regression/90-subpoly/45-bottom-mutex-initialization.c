// PARAM: --set ana.activated[+] subpoly --set ana.activated[-] expRelation --set sem.int.signed_overflow assume_none

                                                                                                          #include <goblint.h>
                                                                                                                   #include <pthread.h>

                                                                                                                            static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
  static int x = 0;
  static int y = 1;

  /* Every published state satisfies y == x + 1. The initial mutex summary is
                                            bottom, so joining it with the initializer must preserve that relation.
                                                                                                            A bottom constructor which returns top destroys it instead. */
                                                                                                                                                               static void *increment(void *arg) {
                                                                                         for (int i = 0; i < 10; i++) {
                                                                                             pthread_mutex_lock(&mutex);
                                                                                             x++;
                                                                                             y++;
                                                                                             pthread_mutex_unlock(&mutex);
                                                                                           }
                                                                                             return 0;
                                                                                       }

                                                                                         int main(void) {
                                                                                         pthread_t thread;
                                                                                         pthread_create(&thread, 0, increment, 0);

                                                                                         pthread_mutex_lock(&mutex);
                                                                                         __goblint_check(y == x + 1); // SUCCESS
                                                                                           pthread_mutex_unlock(&mutex);

                                                                                         pthread_join(thread, 0);
                                                                                         return 0;
                                                                                       }
