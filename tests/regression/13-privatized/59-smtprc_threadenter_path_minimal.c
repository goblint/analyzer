#include <pthread.h>

struct options {
   unsigned short number_of_threads ;
   unsigned short cur_threads ;
};

struct flags {
   unsigned char debug ;
};

struct options o ;
struct flags f ;

int cleaner_start(void)
{
  // make unknown
  int r;
  o.cur_threads = r;
  o.number_of_threads = r;
  f.debug = r;
  return 0;
}

int thread_start()
{
  return 0;
}

pthread_mutex_t main_thread_count_mutex = PTHREAD_MUTEX_INITIALIZER;

int main(int argc , char **argv )
{
  pthread_t c_tid ;
  pthread_create(& c_tid, NULL, & cleaner_start, NULL);


  int x = 0;


  pthread_mutex_lock(& main_thread_count_mutex);
  while ((int )o.cur_threads >= (int )o.number_of_threads) {
    pthread_mutex_unlock(& main_thread_count_mutex);
    if (f.debug) {
      x = 1; // do something
    }
    // missing lock?
  }
  pthread_mutex_unlock(& main_thread_count_mutex);

  // lock gets here with two paths and crashes
  pthread_create(& c_tid, NULL, & thread_start, NULL);
  return (0);
}