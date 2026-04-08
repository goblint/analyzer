// PARAM: --enable ana.int.interval_set
// extracted from SV-COMP task ldv-memsafety/memleaks_test12-2.i with --set exp.unrolling-factor 1

typedef unsigned int size_t;
struct ldv_list_head {
    struct ldv_list_head *next, *prev;
};
struct ldv_list_head ldv_global_msg_list = {&(ldv_global_msg_list), &(ldv_global_msg_list)};
struct ldv_msg {
    void *data;
    struct ldv_list_head list;
};

__inline static void __ldv_list_del(struct ldv_list_head *prev, struct ldv_list_head *next) {
    next->prev = prev;
    prev->next = next;
    return;
}

__inline static void ldv_list_del(struct ldv_list_head *entry) {
    __ldv_list_del(entry->prev, entry->next);
    return;
}

void ldv_msg_free(struct ldv_msg *msg) {
    free(msg->data);
    free((void *)msg);
    return;
}

// ldv_destroy_msgs
void main(void) {
    struct ldv_msg *msg;
    struct ldv_msg *n;
    struct ldv_list_head const *__mptr;
    struct ldv_list_head const *__mptr___0;
    struct ldv_list_head const *__mptr___1;

    __mptr = (struct ldv_list_head const *)ldv_global_msg_list.next;
    msg = (struct ldv_msg *)((char *)__mptr - (size_t)(&((struct ldv_msg *)0)->list));
    __mptr___0 = (struct ldv_list_head const *)msg->list.next;
    n = (struct ldv_msg *)((char *)__mptr___0 - (size_t)(&((struct ldv_msg *)0)->list));

    if (!((unsigned long)(&msg->list) != (unsigned long)(&ldv_global_msg_list))) { // NOWARN
        goto loop_end;
    }

    ldv_list_del(&msg->list);
    ldv_msg_free(msg);
    msg = n;
    __mptr___1 = (struct ldv_list_head const *)n->list.next;
    n = (struct ldv_msg *)((char *)__mptr___1 - (size_t)(&((struct ldv_msg *)0)->list));

    loop_continue_0: /* CIL Label */;
        {
            while (1) {
            while_continue: /* CIL Label */;
                if (!((unsigned long)(&msg->list) != (unsigned long)(&ldv_global_msg_list))) {
                    goto while_break;
                }

                ldv_list_del(&msg->list);
                ldv_msg_free(msg);
                msg = n;
                __mptr___1 = (struct ldv_list_head const *)n->list.next;
                n = (struct ldv_msg *)((char *)__mptr___1 - (size_t)(&((struct ldv_msg *)0)->list));
            }
        while_break: /* CIL Label */;
        }
    loop_end: /* CIL Label */;
    return;
}