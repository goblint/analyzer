typedef union {
   char c[4] ; // c needs to be at least as big as l
   long l ;
} u;

u uv;

int main(){
  __goblint_check(1); // reachable, formerly TERM
  return 0;
}
