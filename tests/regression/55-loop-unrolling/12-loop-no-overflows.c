// PARAM: --enable ana.int.interval_set
// extracted from SV-COMP task ldv-memsafety/memleaks_test12-2.i

typedef unsigned int size_t;
struct ldv_list_head {
    struct ldv_list_head *next, *prev;
};
struct ldv_list_head ldv_global_msg_list = {&(ldv_global_msg_list), &(ldv_global_msg_list)};
struct ldv_msg {
    void *data;
    struct ldv_list_head list;
};

static inline void __ldv_list_del(struct ldv_list_head *prev, struct ldv_list_head *next) {
    next->prev = prev;
    prev->next = next;
}

static inline void ldv_list_del(struct ldv_list_head *entry) {
    __ldv_list_del(entry->prev, entry->next);
}

void ldv_msg_free(struct ldv_msg *msg) {
    free(msg->data);
    free(msg);
}

// ldv_destroy_msgs
void main(void) {
    struct ldv_msg *msg;
    struct ldv_msg *n;
    for (msg = ({ const typeof( ((typeof(*msg) *)0)->list ) *__mptr = ((&ldv_global_msg_list)->next); (typeof(*msg) *)( (char *)__mptr - ((size_t) &((typeof(*msg) *)0)->list) ); }), n = ({ const typeof( ((typeof(*(msg)) *)0)->list ) *__mptr = ((msg)->list.next); (typeof(*(msg)) *)( (char *)__mptr - ((size_t) &((typeof(*(msg)) *)0)->list) ); }); &msg->list != (&ldv_global_msg_list); msg = n, n = ({ const typeof( ((typeof(*(n)) *)0)->list ) *__mptr = ((n)->list.next); (typeof(*(n)) *)( (char *)__mptr - ((size_t) &((typeof(*(n)) *)0)->list) ); })) // NOWARN
    {
        ldv_list_del(&msg->list);
        ldv_msg_free(msg);
    }
}