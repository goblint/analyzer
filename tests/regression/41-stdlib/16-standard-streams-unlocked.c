#define _GNU_SOURCE
#include <stdio.h>
#include <pthread.h>

// Exercise the library specifications rather than SDK inline stdio bodies.
// macOS has no declaration of the GNU fputs_unlocked extension.
#undef putc_unlocked
extern int putc_unlocked(int, FILE *);
extern int fputs_unlocked(const char *, FILE *);

// fputs_unlocked takes no lock on the stream, so from two
// threads they race on the buffer attached to stdout. Under flockfile, which
// takes the lock on the stream, putc_unlocked on stderr does not race.
static char out[BUFSIZ];
static char err[BUFSIZ];

void *t_fun(void *arg) {
  fputs_unlocked("a", stdout); // RACE
  flockfile(stderr);
  putc_unlocked('b', stderr); // NORACE
  funlockfile(stderr);
  return 0;
}

int main(void) {
  setvbuf(stdout, out, _IOFBF, sizeof out);
  setvbuf(stderr, err, _IOFBF, sizeof err);
  pthread_t id;
  pthread_create(&id, 0, t_fun, 0);
  fputs_unlocked("c", stdout); // RACE
  flockfile(stderr);
  putc_unlocked('d', stderr); // NORACE
  fputs("e", stderr); // NORACE
  funlockfile(stderr);
  pthread_join(id, 0);
  return 0;
}
