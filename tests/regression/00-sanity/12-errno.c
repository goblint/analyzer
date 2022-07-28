#include <errno.h>
#include <assert.h>

int main(){
	errno = 1;
  assert(errno); // UNKNOWN!
	return 0;
}
