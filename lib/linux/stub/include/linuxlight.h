#define NULL ((void *)0)

struct mutex { int state; };
#define DEFINE_MUTEX(mutexname)  struct mutex mutexname = {0};
extern void mutex_lock(struct mutex *lock);
extern void mutex_unlock(struct mutex *lock);
extern void mutex_init(struct mutex *lock);

struct list_head {
  struct list_head *next, *prev;
};

#define LIST_HEAD_INIT(name) { &(name), &(name) }
#define LIST_HEAD(name) struct list_head name = LIST_HEAD_INIT(name)

static inline void __list_add(struct list_head *new, 
    struct list_head *prev, struct list_head *next) {
  next->prev = new;
  new->next = next;
  new->prev = prev;
  prev->next = new;
}

static inline void list_add(struct list_head *new, struct list_head *head) {
  __list_add(new, head, head->next);
}

static inline void __list_del(struct list_head * prev, struct list_head * next)
{
	next->prev = prev;
	prev->next = next;
}

static inline void list_del(struct list_head *entry)
{
	__list_del(entry->prev, entry->next);
	entry->next = NULL;
	entry->prev = NULL;
}

#define offsetof(TYPE, MEMBER) ((unsigned int) &((TYPE *)0)->MEMBER)
#define container_of(ptr, type, member) ({			\
	(type *)( (char *) ptr - offsetof(type,member) );})
#define list_entry(ptr, type, member) \
	container_of(ptr, type, member)
#define prefetch(x) __builtin_prefetch(x)
#define list_for_each_entry(pos, head, member)				\
	for (pos = list_entry((head)->next, typeof(*pos), member);	\
	     prefetch(pos->member.next), &pos->member != (head); 	\
	     pos = list_entry(pos->member.next, typeof(*pos), member))
