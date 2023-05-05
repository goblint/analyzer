//PARAM: --set ana.modular.funs "['append_new']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval

#include<goblint.h>
#include<stdlib.h>

typedef struct node {
	int value;
	struct node* next;
} node_t;

void append_new(node_t *n){
	if(n == NULL)
		return;
	node_t *new = malloc(sizeof(node_t));
	new->value = 0;
	new->next = NULL;

	while(n->next != NULL){
		n = n->next;
	}

	n->next = new;
	__goblint_check(n->next == new); //UNKNOWN
}


int length(node_t *list){
	int i = 0;
	while(list != NULL){
		list = list->next;
		i++;
	}
	return i;
}


int sum(node_t *list){
	int sum = 0;
	while(list != NULL){
		sum = sum + list->value;
		list = list->next;
	}
	return sum;
}

int main(){
	node_t n = {0, NULL};
	append_new(&n);

	int len = 10;

	for(int i = 0; i < len; i++){
		append_new(&n);
	}

	int l = 0;
	int s = 0;

	l = length(&n);

	__goblint_check(l >= 0); //UNKNOWN
	__goblint_check(l <= 10); //UNKNOWN

	s = sum(&n);

	__goblint_check(s == 0);
}