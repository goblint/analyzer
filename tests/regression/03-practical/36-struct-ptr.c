#include <goblint.h>
struct aws_al {
	unsigned long current_size;
};

struct aws_pq {
	int pred;
	struct aws_al container;
};

int main() {
	struct aws_pq queue = { 0, { 0}};
	struct aws_al *const list = &queue.container;

	if (list->current_size == 0UL) {
		if (list->current_size == 0UL) {
			__goblint_check(1); //REACHABLE
		}
	}
}
