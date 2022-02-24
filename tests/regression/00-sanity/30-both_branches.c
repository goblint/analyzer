
union bloirg {
   char *zToken ;
   int iValue ;
};


struct Sqlite3Config {
   union bloirg m ;
   int* blarg;
};

struct Sqlite3Config sqlite3Config;


int main(int argc , char **argv )
{
   int rc = 0;

  if ((unsigned long )sqlite3Config.blarg == (unsigned long )((void *(*)(int  ))0)) {
      rc = 5;
  }

  rc = 1;
  assert(1);

   int r = 8;
}
