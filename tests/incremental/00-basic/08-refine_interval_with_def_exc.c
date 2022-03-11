struct input_state;
typedef struct input_state input_state;
struct input_state {
   char buf[512] ;
   int valid ;
};

char *input_get_line(input_state *state )
{ char *result ;
  int done ;
  char *start ;
  char *newline ;
  int n ;

  {
  result = (char *)((void *)0);
  while (! done) {
    start = & state->buf[state->valid];
    if (newline != ((void *)0)) {
      result = start;
      state->valid = (newline - state->buf) + 2;
    } else {
      if (state->valid < 511) {
        if (n <= 0) {
          result = (char *)((void *)0);
        }
      }
    }
  }
  return (result);
}
}

int main()
{
  input_state state ;
  char *line ;
  state.valid = 0;
  while (1) {
    line = input_get_line(& state);
  }
  return 0;
}
