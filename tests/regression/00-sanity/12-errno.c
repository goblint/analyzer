#include <errno.h>
#include <assert.h>

int main(){
	errno = 1;
  __goblint_check(errno); // UNKNOWN!
	return 0;
}
