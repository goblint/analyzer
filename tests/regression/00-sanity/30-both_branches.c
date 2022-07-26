// PARAM: --set ana.base.privatization none --enable exp.earlyglobs --disable exp.fast_global_inits
#include <assert.h>

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

  assert(sqlite3Config.blarg == 0);
  assert(sqlite3Config.m.iValdue == 0);

  if ((unsigned long )sqlite3Config.blarg == (unsigned long )((void *(*)(int  ))0)) {
      rc = 5;
  }

  assert(1);
}
