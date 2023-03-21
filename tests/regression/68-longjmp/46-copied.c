// PARAM: --enable ana.int.interval --enable ana.int.enums --set ana.activated[+] expsplit --disable warn.deadcode
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
// #include <goblint.h>

struct buf_struct {
   jmp_buf buf;
};

struct buf_struct env_buffer;
struct buf_struct buffer2;
int global = 0;

int main () {
   int val;
   __goblint_check(global == 0);

   if(setjmp(env_buffer.buf)) { //NOWARN
      return 0;
   }

   buffer2 = env_buffer;

   longjmp(buffer2.buf,42); //WARN

   return(0);
}
