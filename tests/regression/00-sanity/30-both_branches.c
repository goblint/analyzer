// PARAM: --enable exp.earlyglobs --disable exp.fast_global_inits
#include <goblint.h>

union bloirg {
   int iValdue ;
};

struct Sqlite3Config {
   union bloirg m ;
   int* blarg;
};

struct Sqlite3Config sqlite3Config;


int main(int argc , char **argv )
{
  int rc = 0;

  __goblint_check(sqlite3Config.blarg == 0);
  __goblint_check(sqlite3Config.m.iValdue == 0);

  if ((unsigned long )sqlite3Config.blarg == (unsigned long )((void *(*)(int  ))0)) {
      rc = 5;
  }

  __goblint_check(1);
}
