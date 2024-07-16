// PARAM: --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
// Simplified example from the silver searcher
#include <pthread.h>

typedef struct {
  char *color_match;
} cli_options;

struct print_context {
  char **context_prev_lines;
};

extern struct print_context *get_print_context();

cli_options opts;

void *t_fun(void *arg) {
  opts.color_match = "\033[30;43m"; // RACE!
  return NULL;
}

int main(void) {
  struct print_context *s;
  pthread_t id;

  s = get_print_context();
  pthread_create(&id,NULL,t_fun,NULL);
  char *x = s->context_prev_lines[2]; // RACE!
  return 0;
}
