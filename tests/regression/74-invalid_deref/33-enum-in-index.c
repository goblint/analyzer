//PARAM: --set ana.activated[+] memOutOfBounds
//NOCRASH (had invalid ikind exceptions earlier)
#include <stdlib.h>

typedef enum {
    ITEM_PREV,
    ITEM_NEXT
} direction_t;

struct s {
    int head[2];
};


int main()
{
    struct s* item = malloc(sizeof(struct s));
    direction_t link_field = ITEM_NEXT;
    item->head[link_field] = 0;
    return 0;
}
