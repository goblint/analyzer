// SKIP PARAM: --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
// SV-COMP marshaling doesn't work
extern void abort(void);
extern void __assert_fail(const char *, const char *, unsigned int, const char *) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));
void reach_error() { __assert_fail("0", "ddv_machzwd_inb.i", 3, "reach_error"); }

/* SUPPOSED TO SUCCEED */


extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}
void __VERIFIER_assert(int expression, char* x) { if (!expression) { ERROR: {reach_error();abort();}}; return; }







int current_execution_context;
static inline void assert_context_process()
{
}

static inline void assert_context_interrupt()
{
}



int (* _ddv_module_init)(void);
void (* _ddv_module_exit)(void);

int call_ddv();












typedef int __kernel_key_t;
typedef int __kernel_mqd_t;




typedef unsigned long __kernel_ino_t;
typedef unsigned short __kernel_mode_t;
typedef unsigned short __kernel_nlink_t;
typedef long __kernel_off_t;
typedef int __kernel_pid_t;
typedef unsigned short __kernel_ipc_pid_t;
typedef unsigned short __kernel_uid_t;
typedef unsigned short __kernel_gid_t;
typedef unsigned int __kernel_size_t;
typedef int __kernel_ssize_t;
typedef int __kernel_ptrdiff_t;
typedef long __kernel_time_t;
typedef long __kernel_suseconds_t;
typedef long __kernel_clock_t;
typedef int __kernel_timer_t;
typedef int __kernel_clockid_t;
typedef int __kernel_daddr_t;
typedef char * __kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;
typedef unsigned int __kernel_uid32_t;
typedef unsigned int __kernel_gid32_t;

typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;
typedef unsigned short __kernel_old_dev_t;



typedef unsigned short umode_t;

typedef __signed__ char __s8;
typedef unsigned char __u8;

typedef __signed__ short __s16;
typedef unsigned short __u16;

typedef __signed__ int __s32;
typedef unsigned int __u32;

typedef __signed__ long long __s64;
typedef unsigned long long __u64;



typedef signed char s8;
typedef unsigned char u8;

typedef signed short s16;
typedef unsigned short u16;

typedef signed int s32;
typedef unsigned int u32;

typedef signed long long s64;
typedef unsigned long long u64;

typedef u32 dma_addr_t;
typedef u64 dma64_addr_t;



typedef __u32 __kernel_dev_t;

typedef __kernel_dev_t dev_t;
typedef __kernel_ino_t ino_t;
typedef __kernel_mode_t mode_t;
typedef __kernel_nlink_t nlink_t;
typedef __kernel_off_t off_t;
typedef __kernel_pid_t pid_t;
typedef __kernel_daddr_t daddr_t;
typedef __kernel_key_t key_t;
typedef __kernel_suseconds_t suseconds_t;
typedef __kernel_timer_t timer_t;
typedef __kernel_clockid_t clockid_t;
typedef __kernel_mqd_t mqd_t;

typedef __kernel_uid32_t uid_t;
typedef __kernel_gid32_t gid_t;
typedef __kernel_uid16_t uid16_t;
typedef __kernel_gid16_t gid16_t;


typedef long long loff_t;







typedef __kernel_size_t size_t;




typedef __kernel_ssize_t ssize_t;




typedef __kernel_ptrdiff_t ptrdiff_t;




typedef __kernel_time_t time_t;




typedef __kernel_clock_t clock_t;




typedef __kernel_caddr_t caddr_t;




typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;


typedef unsigned char unchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;


typedef __u8 uint8_t;
typedef __u16 uint16_t;
typedef __u32 uint32_t;

typedef __u64 uint64_t;
typedef __u64 u_int64_t;


typedef unsigned gfp_t;


typedef unsigned long sector_t;



typedef unsigned long blkcnt_t;


typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;
typedef __u64 __le64;
typedef __u64 __be64;
void barrier(void);
int capable(int cap);









struct timespec {
 time_t tv_sec;
 long tv_nsec;
};

struct timeval {
    time_t tv_sec;
    suseconds_t tv_usec;
};
void do_gettimeofday(struct timeval *tv);

unsigned long jiffies;
static inline unsigned int jiffies_to_msecs(const unsigned long j)
{

 return (1000L / 100) * j;





}

static inline unsigned int jiffies_to_usecs(const unsigned long j)
{

 return (1000000L / 100) * j;





}

static inline unsigned long msecs_to_jiffies(const unsigned int m)
{
 if (m > jiffies_to_msecs(((~0UL >> 1)-1)))
  return ((~0UL >> 1)-1);

 return (m + (1000L / 100) - 1) / (1000L / 100);





}

static inline unsigned long usecs_to_jiffies(const unsigned int u)
{
 if (u > jiffies_to_usecs(((~0UL >> 1)-1)))
  return ((~0UL >> 1)-1);

 return (u + (1000000L / 100) - 1) / (1000000L / 100);





}













struct timer_list {
    unsigned long expires;
    void (*function)(unsigned long);
    unsigned long data;

    short __ddv_active;
    short __ddv_init;
};
void init_timer(struct timer_list * timer);
void add_timer_on(struct timer_list *timer, int cpu);
void add_timer(struct timer_list *timer);
int del_timer(struct timer_list * timer);
int mod_timer(struct timer_list *timer, unsigned long expires);









typedef struct {
    int init;
    int locked;
} spinlock_t;

typedef struct {
  int something;
} rwlock_t;



void spin_lock_init(spinlock_t *);
void spin_lock(spinlock_t *);
void spin_lock_irqsave(spinlock_t *, unsigned long);
void spin_lock_irq(spinlock_t *);
void spin_lock_bh(spinlock_t *);

void spin_unlock(spinlock_t *);
void spin_unlock_irqrestore(spinlock_t *, unsigned long);
void spin_unlock_irq(spinlock_t *);
void spin_unlock_bh(spinlock_t *);























void rmb(void);
void read_barrier_depends(void);
void wmb(void);
void mb(void);

struct list_head {
 struct list_head *next, *prev;
};







static inline void INIT_LIST_HEAD(struct list_head *list)
{
 list->next = list;
 list->prev = list;
}
static inline void __list_add(struct list_head *new,
         struct list_head *prev,
         struct list_head *next)
{
 next->prev = new;
 new->next = next;
 new->prev = prev;
 prev->next = new;
}
static inline void list_add(struct list_head *new, struct list_head *head)
{
 __list_add(new, head, head->next);
}
static inline void __list_del(struct list_head * prev, struct list_head * next)
{
 next->prev = prev;
 prev->next = next;
}





static inline void list_del_init(struct list_head *entry)
{
 __list_del(entry->prev, entry->next);
 INIT_LIST_HEAD(entry);
}





static inline int list_empty(const struct list_head *head)
{
 return head->next == head;
}







struct task_struct;

struct task_struct *get_current(void);

struct __wait_queue {
    int something;
};
typedef struct __wait_queue wait_queue_t;

struct __wait_queue_head {
    int number_process_waiting;
    int wakeup;

  int init;
};
typedef struct __wait_queue_head wait_queue_head_t;
void init_waitqueue_head(wait_queue_head_t *q);

void prepare_to_wait(wait_queue_head_t *q, wait_queue_t *wait, int state);

void finish_wait(wait_queue_head_t *q, wait_queue_t *wait);


void wake_up(wait_queue_head_t *q);

void wake_up_all(wait_queue_head_t *q);

void wake_up_interruptible(wait_queue_head_t *q);


void add_wait_queue(wait_queue_head_t *q, wait_queue_t * wait);

void add_wait_queue_exclusive(wait_queue_head_t *q, wait_queue_t * wait);

void remove_wait_queue(wait_queue_head_t *q, wait_queue_t * wait);


int waitqueue_active(wait_queue_head_t *q);


void sleep_on(wait_queue_head_t *q);

void interruptible_sleep_on(wait_queue_head_t *q);














struct __pthread_t_struct
{
  int id;
};

struct __pthread_attr_t_struct
{
  int dummy;
};

struct __pthread_mutex_t_struct
{
  _Bool locked;
};

struct __pthread_mutexattr_t_struct
{
  int dummy;
};

struct __pthread_spinlock_t_struct
{
  int dummy;
};

struct __pthread_barrier_t_struct
{
  int dummy;
};

struct __pthread_barrierattr_t_struct
{
  int dummy;
};

typedef struct __pthread_t_struct pthread_t;
typedef struct __pthread_attr_t_struct pthread_attr_t;
typedef struct __pthread_mutex_t_struct pthread_mutex_t;
typedef struct __pthread_mutexattr_t_struct pthread_mutexattr_t;
typedef struct __pthread_spinlock_t_struct pthread_spinlock_t;
typedef struct __pthread_barrier_t_struct pthread_barrier_t;
typedef struct __pthread_barrierattr_t_struct pthread_barrierattr_t;







enum
{
  PTHREAD_CREATE_JOINABLE,

  PTHREAD_CREATE_DETACHED

};

enum
{
  PTHREAD_INHERIT_SCHED,

  PTHREAD_EXPLICIT_SCHED

};

enum
{
  PTHREAD_SCOPE_SYSTEM,

  PTHREAD_SCOPE_PROCESS

};

enum
{
  PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_ADAPTIVE_NP,
  PTHREAD_MUTEX_NORMAL = PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE = PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK = PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL
};

enum
{
  PTHREAD_PROCESS_PRIVATE,

  PTHREAD_PROCESS_SHARED

};

enum
{
  PTHREAD_RWLOCK_PREFER_READER_NP,
  PTHREAD_RWLOCK_PREFER_WRITER_NP,
  PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP,
  PTHREAD_RWLOCK_DEFAULT_NP = PTHREAD_RWLOCK_PREFER_WRITER_NP
};






enum
{
  PTHREAD_CANCEL_ENABLE,

  PTHREAD_CANCEL_DISABLE

};
enum
{
  PTHREAD_CANCEL_DEFERRED,

  PTHREAD_CANCEL_ASYNCHRONOUS

};


pthread_t __VERIFIER_nondet_pthread_t();

extern inline int pthread_create(
  pthread_t *__threadp,
  __const pthread_attr_t *__attr,
  void *(*__start_routine) (void *),
  void *__arg);

struct sched_param;

extern pthread_t pthread_self(void);
extern int pthread_equal(pthread_t __thread1, pthread_t __thread2);
extern void pthread_exit(void *__retval);
extern int pthread_join(pthread_t __th, void **__thread_return);
extern int pthread_detach(pthread_t __th);
extern int pthread_attr_init(pthread_attr_t *__attr);
extern int pthread_attr_destroy(pthread_attr_t *__attr);
extern int pthread_attr_setdetachstate(pthread_attr_t *__attr, int __detachstate);
extern int pthread_attr_getdetachstate(__const pthread_attr_t *__attr, int *__detachstate);
extern int pthread_attr_setschedparam(pthread_attr_t *__attr, __const struct sched_param *__param);
extern int pthread_attr_getschedparam(__const pthread_attr_t *__attr, struct sched_param *__param);
extern int pthread_attr_setschedpolicy(pthread_attr_t *__attr, int __policy);
extern int pthread_attr_getschedpolicy(__const pthread_attr_t *__attr, int *__policy);
extern int pthread_attr_setinheritsched(pthread_attr_t *__attr, int __inherit);
extern int pthread_attr_getinheritsched(__const pthread_attr_t *__attr, int *__inherit);
extern int pthread_attr_setscope(pthread_attr_t *__attr, int __scope);
extern int pthread_attr_getscope(__const pthread_attr_t *__attr, int *__scope);
extern int pthread_attr_setguardsize(pthread_attr_t *__attr, size_t __guardsize);
extern int pthread_attr_getguardsize(__const pthread_attr_t *__attr, size_t *__guardsize);
extern int pthread_attr_setstackaddr(pthread_attr_t *__attr, void *__stackaddr);
extern int pthread_attr_getstackaddr(__const pthread_attr_t *__attr, void **__stackaddr);
extern int pthread_attr_setstack(pthread_attr_t *__attr, void *__stackaddr, size_t __stacksize);
extern int pthread_attr_getstack(__const pthread_attr_t *__attr,
     void **__stackaddr,
     size_t *__stacksize);
extern int pthread_attr_setstacksize(pthread_attr_t *__attr, size_t __stacksize);
extern int pthread_attr_getstacksize(__const pthread_attr_t *, size_t *__stacksize);

extern int pthread_getattr_np(pthread_t __th, pthread_attr_t *__attr);

extern int pthread_setschedparam(pthread_t __target_thread, int __policy,
     __const struct sched_param *__param);

extern int pthread_getschedparam(pthread_t __target_thread,
     int *__policy,
     struct sched_param *__param);

extern int pthread_getconcurrency(void);
extern int pthread_setconcurrency(int __level);
extern int pthread_yield(void);

extern inline int pthread_mutex_init(
  pthread_mutex_t *__mutex,
  __const pthread_mutexattr_t *__mutex_attr)
{
  pthread_mutex_t i={0};
  *__mutex=i;
}

extern inline int pthread_mutex_destroy(pthread_mutex_t *__mutex)
{
}

extern int pthread_mutex_trylock(pthread_mutex_t *__mutex);

void __VERIFIER_atomic_begin();
void __VERIFIER_atomic_end();

extern inline int pthread_mutex_lock(pthread_mutex_t *__mutex)
{
  __VERIFIER_HIDE:
  __VERIFIER_atomic_begin();
  assume_abort_if_not(!__mutex->locked);
  __mutex->locked=1;
  __VERIFIER_atomic_end();
  return 0;
}

extern inline int pthread_mutex_unlock(pthread_mutex_t *__mutex)
{
  __mutex->locked=0;
  return 0;
}

extern int pthread_spin_init(pthread_spinlock_t *__lock, int __pshared);
extern int pthread_spin_destroy(pthread_spinlock_t *__lock);
extern int pthread_spin_lock(pthread_spinlock_t *__lock);
extern int pthread_spin_trylock(pthread_spinlock_t *__lock);
extern int pthread_spin_unlock(pthread_spinlock_t *__lock);
extern int pthread_barrier_init(pthread_barrier_t *__barrier,
                                __const pthread_barrierattr_t *__attr, unsigned int __count);

extern int pthread_barrier_destroy(pthread_barrier_t *__barrier);
extern int pthread_barrierattr_init(pthread_barrierattr_t *__attr);
extern int pthread_barrierattr_destroy(pthread_barrierattr_t *__attr);
extern int pthread_barrierattr_getpshared(__const pthread_barrierattr_t *__attr,
       int *__pshared);

extern int pthread_barrierattr_setpshared(pthread_barrierattr_t *__attr, int __pshared);
extern int pthread_barrier_wait(pthread_barrier_t *__barrier);

extern int pthread_setcancelstate(int __state, int *__oldstate);
extern int pthread_setcanceltype(int __type, int *__oldtype);
extern int pthread_cancel(pthread_t __cancelthread);
extern void pthread_testcancel(void);

extern int pthread_atfork(void (*__prepare)(void),
     void (*__parent)(void),
     void (*__child)(void));

extern void pthread_kill_other_threads_np(void);



void *malloc(size_t size);




short __VERIFIER_nondet_short();
unsigned short __VERIFIER_nondet_ushort();
int __VERIFIER_nondet_int();
unsigned int __VERIFIER_nondet_uint();
long __VERIFIER_nondet_long();
unsigned long __VERIFIER_nondet_ulong();
char __VERIFIER_nondet_char();
unsigned char __VERIFIER_nondet_uchar();
unsigned __VERIFIER_nondet_unsigned();
loff_t __VERIFIER_nondet_loff_t();
size_t __VERIFIER_nondet_size_t();
sector_t __VERIFIER_nondet_sector_t();
u32 __VERIFIER_nondet_u32();
char * __VERIFIER_nondet_pchar();
_Bool __VERIFIER_nondet_bool();




void noop();





void __VERIFIER_atomic_begin();
void __VERIFIER_atomic_end();

typedef int atomic_t;
static __inline__ void atomic_add(int i, atomic_t *v)
{
    (*v) = (*v) + i;
}
static __inline__ void atomic_sub(int i, atomic_t *v)
{
    (*v) = (*v) - i;
}







static __inline__ void atomic_inc(atomic_t *v)
{
    (*v)++;
}







static __inline__ void atomic_dec(atomic_t *v)
{
    (*v)--;
}
static __inline__ int atomic_dec_and_test(atomic_t *v)
{
    int ret;

 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();
    (*v)--;
    if ((*v) == 0) {
 ret = 1;
    } else {
 ret = 0;
    };
    __VERIFIER_atomic_end();

    return ret;
}
static __inline__ int atomic_inc_and_test(atomic_t *v)
{
    int ret;

 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();
    (*v)++;
    if ((*v) == 0) {
 ret = 1;
    } else {
 ret = 0;
    };
    __VERIFIER_atomic_end();

    return ret;
}
static __inline__ int atomic_add_negative(int i, atomic_t *v)
{
   int ret;

 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();
    (*v) = (*v) + i;
    if ((*v) < 0) {
 ret = 1;
    } else {
 ret = 0;
    }
    __VERIFIER_atomic_end();

    return ret;
}
static __inline__ int atomic_add_return(int i, atomic_t *v)
{
    return (*v) + i;
}

static __inline__ int atomic_sub_return(int i, atomic_t *v)
{
    return (*v) - i;
}


unsigned long __get_free_pages(gfp_t gfp_mask, unsigned int order);

unsigned long __get_free_page(gfp_t gfp_mask);

unsigned long get_zeroed_page(gfp_t gfp_mask);


void free_pages(unsigned long addr, unsigned int order);

void free_page(unsigned long addr);


static struct page *alloc_pages_node(int nid, gfp_t gfp_mask,
         unsigned int order);

struct page * alloc_pages(gfp_t gfp_mask, unsigned int order);

struct page * alloc_page(gfp_t gfp_mask);



void kfree(const void *);

void *kmalloc(size_t size, gfp_t flags);

void *kzalloc(size_t size, gfp_t flags);

unsigned int ksize(const void *);









void *memset(void *s, int c, size_t n);

extern char * strcpy(char *,const char *);
extern char * strncpy(char *,const char *, __kernel_size_t);
size_t strlcpy(char *, const char *, size_t);
extern char * strcat(char *, const char *);
extern char * strncat(char *, const char *, __kernel_size_t);
extern size_t strlcat(char *, const char *, __kernel_size_t);
extern int strcmp(const char *,const char *);
extern int strncmp(const char *,const char *,__kernel_size_t);
extern int strnicmp(const char *, const char *, __kernel_size_t);
extern char * strchr(const char *,int);
extern char * strnchr(const char *, size_t, int);
extern char * strrchr(const char *,int);
extern char * strstrip(char *);
extern char * strstr(const char *,const char *);
extern __kernel_size_t strlen(const char *);
extern __kernel_size_t strnlen(const char *,__kernel_size_t);
extern char * strpbrk(const char *,const char *);
extern char * strsep(char **,const char *);
extern __kernel_size_t strspn(const char *,const char *);
extern __kernel_size_t strcspn(const char *,const char *);

extern void * memset(void *,int,__kernel_size_t);
extern void * memcpy(void *,const void *,__kernel_size_t);
extern void * memmove(void *,const void *,__kernel_size_t);
extern void * memscan(void *,int,__kernel_size_t);
extern int memcmp(const void *,const void *,__kernel_size_t);
extern void * memchr(const void *,int,__kernel_size_t);















typedef unsigned long old_sigset_t;

typedef struct {
 unsigned long sig[(64 / 32)];
} sigset_t;



typedef struct siginfo {
    int something;
} siginfo_t;










void set_bit(int nr, unsigned long * addr);

int test_and_set_bit(int nr, unsigned long * addr);
int test_and_clear_bit(int nr, unsigned long * addr);
int test_and_change_bit(int nr, unsigned long* addr);
int test_bit(int nr, const void * addr);
void clear_bit(int nr, volatile unsigned long * addr);
int find_first_zero_bit(const unsigned long *addr, unsigned size);
static inline void sigfillset(sigset_t *set)
{
 switch ((64 / 32)) {
 default:
  memset(set, -1, sizeof(sigset_t));
  break;
 case 2: set->sig[1] = -1;
 case 1: set->sig[0] = -1;
  break;
 }
}



static inline void sigaddsetmask(sigset_t *set, unsigned long mask)
{
 set->sig[0] |= mask;
}

static inline void sigdelsetmask(sigset_t *set, unsigned long mask)
{
 set->sig[0] &= ~mask;
}

static inline int sigtestsetmask(sigset_t *set, unsigned long mask)
{
 return (set->sig[0] & mask) != 0;
}
struct sighand_struct {
    spinlock_t siglock;
};

struct task_struct {
    long state;
    pid_t pid;
    char comm[16];

    sigset_t blocked, real_blocked;

    struct sighand_struct *sighand;
};



int signal_pending(struct task_struct *p);


void schedule(void);

long schedule_timeout(long timeout);

int need_resched(void);


extern int dequeue_signal(struct task_struct *tsk, sigset_t *mask, siginfo_t *info);


void yield(void);


void set_current_state(int);


extern int kill_proc(pid_t, int, int);

extern void recalc_sigpending(void);




















unsigned long simple_strtoul(const char *,char **,unsigned int);
long simple_strtol(const char *,char **,unsigned int);
unsigned long long simple_strtoull(const char *,char **,unsigned int);
long long simple_strtoll(const char *,char **,unsigned int);

int printk(const char * fmt, ...);
int sprintf(char * buf, const char * fmt, ...);
int snprintf(char * buf, size_t size, const char * fmt, ...);

extern int cond_resched(void);

extern int get_option(char **str, int *pint);
extern char *get_options(const char *str, int nints, int *ints);


extern enum system_states {
 SYSTEM_BOOTING,
 SYSTEM_RUNNING,
 SYSTEM_HALT,
 SYSTEM_POWER_OFF,
 SYSTEM_RESTART,
 SYSTEM_SUSPEND_DISK,
} system_state;
extern void dump_stack(void);




struct kstat {
 unsigned long ino;
 dev_t dev;

 unsigned int nlink;


 dev_t rdev;
 loff_t size;



 unsigned long blksize;
 unsigned long blocks;
};
struct module {
    int something;
};



void __module_get(struct module *module);







struct miscdevice {
    int minor;
    const char *name;
    struct file_operations *fops;
};

extern int misc_register(struct miscdevice * misc);
extern int misc_deregister(struct miscdevice * misc);






extern unsigned int __invalid_size_argument_for_IOC;




struct watchdog_info {
 __u32 options;
 __u32 firmware_version;
 __u8 identity[32];
};



struct dentry {
    struct inode *d_inode;

};







struct semaphore {
    int init;
    int locked;
};
void sema_init(struct semaphore *sem, int val);

void init_MUTEX(struct semaphore * sem);

void init_MUTEX_LOCKED(struct semaphore * sem);

void down(struct semaphore * sem);

int down_interruptible(struct semaphore * sem);

int down_trylock(struct semaphore * sem);

void up(struct semaphore * sem);








static inline void *ERR_PTR(long error)
{
 return (void *) error;
}

static inline long PTR_ERR(const void *ptr)
{
 return (long) ptr;
}

static inline long IS_ERR(const void *ptr)
{
 return ((unsigned long)ptr) >= (unsigned long)-4095;
}
struct hd_geometry;
struct iovec;
struct poll_table_struct;
struct vm_area_struct;

struct page;
struct seq_file;

struct address_space {
    struct inode *host;
};

struct file {
    struct dentry *f_dentry;
    struct file_operations *f_op;
    atomic_t f_count;
    unsigned int f_flags;
    mode_t f_mode;
    loff_t f_pos;
    void *private_data;
    struct address_space *f_mapping;
};

struct block_device {
    struct inode * bd_inode;
    struct gendisk * bd_disk;

    struct block_device * bd_contains;
    unsigned bd_block_size;
};

struct inode {
    umode_t i_mode;

    struct block_device *i_bdev;
    dev_t i_rdev;
    loff_t i_size;
    struct cdev *i_cdev;
};

typedef struct {
 size_t written;
 size_t count;
} read_descriptor_t;

typedef int (*filldir_t)(void *, const char *, int, loff_t, ino_t, unsigned);
typedef int (*read_actor_t)(read_descriptor_t *, struct page *, unsigned long, unsigned long);

struct file_lock {
    int something;
};

struct file_operations {
    struct module *owner;
    loff_t (*llseek) (struct file *, loff_t, int);
    ssize_t (*read) (struct file *, char *, size_t, loff_t *);


    ssize_t (*write) (struct file *, const char *, size_t, loff_t *);


    int (*readdir) (struct file *, void *, filldir_t);
    unsigned int (*poll) (struct file *, struct poll_table_struct *);
    int (*ioctl) (struct inode *, struct file *, unsigned int, unsigned long);
    long (*unlocked_ioctl) (struct file *, unsigned int, unsigned long);
    long (*compat_ioctl) (struct file *, unsigned int, unsigned long);
    int (*mmap) (struct file *, struct vm_area_struct *);
    int (*open) (struct inode *, struct file *);
    int (*flush) (struct file *);
    int (*release) (struct inode *, struct file *);
    int (*fsync) (struct file *, struct dentry *, int datasync);


    int (*fasync) (int, struct file *, int);
    int (*lock) (struct file *, int, struct file_lock *);
    ssize_t (*readv) (struct file *, const struct iovec *, unsigned long, loff_t *);
    ssize_t (*writev) (struct file *, const struct iovec *, unsigned long, loff_t *);
    ssize_t (*sendfile) (struct file *, loff_t *, size_t, read_actor_t, void *);
    ssize_t (*sendpage) (struct file *, struct page *, int, size_t, loff_t *, int);
    unsigned long (*get_unmapped_area)(struct file *, unsigned long, unsigned long, unsigned long, unsigned long);
    int (*check_flags)(int);
    int (*dir_notify)(struct file *filp, unsigned long arg);
    int (*flock) (struct file *, int, struct file_lock *);
    int (*open_exec) (struct inode *);
};

struct block_device_operations {
 int (*open) (struct inode *, struct file *);
 int (*release) (struct inode *, struct file *);
 int (*ioctl) (struct inode *, struct file *, unsigned, unsigned long);
 long (*unlocked_ioctl) (struct file *, unsigned, unsigned long);
 long (*compat_ioctl) (struct file *, unsigned, unsigned long);
 int (*direct_access) (struct block_device *, sector_t, unsigned long *);
 int (*media_changed) (struct gendisk *);
 int (*revalidate_disk) (struct gendisk *);
 int (*getgeo)(struct block_device *, struct hd_geometry *);
 struct module *owner;
};


struct fasync_struct {
    int something;
};

extern int fasync_helper(int, struct file *, int, struct fasync_struct **);
int alloc_chrdev_region(dev_t *, unsigned, unsigned, const char *);
int register_chrdev_region(dev_t, unsigned, const char *);
void unregister_chrdev_region(dev_t, unsigned);

int register_chrdev(unsigned int, const char *, struct file_operations *);
int unregister_chrdev(unsigned int, const char *);

int chrdev_open(struct inode *, struct file *);
void chrdev_show(struct seq_file *,off_t);



int register_blkdev(unsigned int, const char *);
int unregister_blkdev(unsigned int, const char *);


void kill_fasync(struct fasync_struct **, int, int);

static inline unsigned iminor(struct inode *inode)
{
 return ((unsigned int) ((inode->i_rdev) & ((1U << 20) - 1)));
}

static inline unsigned imajor(struct inode *inode)
{
 return ((unsigned int) ((inode->i_rdev) >> 20));
}

loff_t no_llseek(struct file *file, loff_t offset, int origin);


int check_disk_change(struct block_device *);






int nonseekable_open(struct inode * inode, struct file * filp);

loff_t i_size_read(struct inode *inode);

int set_blocksize(struct block_device *, int);
struct resource {
 const char *name;
 unsigned long start, end;
 unsigned long flags;

};
struct resource *request_region(unsigned long, unsigned long, const char *);

void release_region(unsigned long, unsigned long);



struct resource *request_mem_region(unsigned long start, unsigned long len, const char *name);

void release_mem_region(unsigned long start, unsigned long len);
struct notifier_block
{
    int (*notifier_call)(struct notifier_block *self, unsigned long, void *);
    struct notifier_block *next;
    int priority;
};
extern int register_reboot_notifier(struct notifier_block *);
extern int unregister_reboot_notifier(struct notifier_block *);






extern void machine_restart(char *cmd);
extern void machine_halt(void);
extern void machine_power_off(void);

extern void machine_shutdown(void);
struct pt_regs;
extern void machine_crash_shutdown(struct pt_regs *);





extern void kernel_shutdown_prepare(enum system_states state);

extern void kernel_restart(char *cmd);
extern void kernel_halt(void);
extern void kernel_power_off(void);

void ctrl_alt_del(void);





extern void emergency_restart(void);





void * phys_to_virt(unsigned long);



void *ioremap(unsigned long offset, unsigned long size);
void *ioremap_nocache(unsigned long offset, unsigned long size);
void iounmap(void *addr);



unsigned char inb(unsigned int);
void outb(unsigned char byte, unsigned int);
unsigned short inw(unsigned int);
void outw(unsigned short word, unsigned int);
unsigned inl(unsigned int);
void outl(unsigned doubleword, unsigned int);



unsigned char inb_p(unsigned int);
void outb_p(unsigned char byte, unsigned int);
unsigned short inw_p(unsigned int);
void outw_p(unsigned short word, unsigned int);
unsigned inl_p(unsigned int);
void outl_p(unsigned doubleword, unsigned int);



unsigned insb(unsigned int, void *addr, unsigned long count);
void outsb(unsigned int, void *addr, unsigned long count);

unsigned insw(unsigned int, void *addr, unsigned long count);
void outsw(unsigned int, void *addr, unsigned long count);

unsigned insl(unsigned int, void *addr, unsigned long count);
void outsl(unsigned int, void *addr, unsigned long count);



unsigned int ioread8(void *addr);
unsigned int ioread16(void *addr);
unsigned int ioread32(void *addr);

void iowrite8(u8 value, void *addr);
void iowrite16(u16 value, void *addr);
void iowrite32(u32 value, void *addr);

void ioread8_rep(void *addr, void *buf, unsigned long count);
void ioread16_rep(void *addr, void *buf, unsigned long count);
void ioread32_rep(void *addr, void *buf, unsigned long count);

void iowrite8_rep(void *addr, const void *buf, unsigned long count);
void iowrite16_rep(void *addr, const void *buf, unsigned long count);
void iowrite32_rep(void *addr, const void *buf, unsigned long count);


unsigned char readb(const void *);
unsigned short readw(const void *);
unsigned int readl(const void *);


void writeb(unsigned char, void *);
void writew(unsigned short, void *);
void writel(unsigned int, void *);








int access_ok(int type, const void *addr, unsigned long size);





int __get_user(int size, void *ptr);

int get_user(int size, const void *ptr);


int __put_user(int size, void *ptr);

int put_user(int size, void *ptr);


unsigned long copy_to_user(void *to, const void *from, unsigned long n);

unsigned long copy_from_user(void *to, void *from, unsigned long n);
static unsigned short zf_readw(unsigned char port)
{
 outb(port, 0x218);
 return inw(0x21A);
}


char _ddv_module_author[] = "Fernando Fuganti <fuganti@conectiva.com.br>";
char _ddv_module_description[] = "MachZ ZF-Logic Watchdog driver";
char _ddv_module_license[] = "GPL";

static int nowayout = 0;
char _ddv_module_param_nowayout [] = "Watchdog cannot be stopped once started (default=CONFIG_WATCHDOG_NOWAYOUT)";



static struct watchdog_info zf_info = {
 .options = 0x8000 | 0x0100,
 .firmware_version = 1,
 .identity = "ZF-Logic watchdog",
};
static int action = 0;
char _ddv_module_param_action [] = "after watchdog resets, generate: 0 = RESET(*)  1 = SMI  2 = NMI  3 = SCI";

static int zf_action = 0x0800;
static unsigned long zf_is_open;
static char zf_expect_close;
static spinlock_t zf_lock;
static spinlock_t zf_port_lock;
static struct timer_list zf_timer;
static unsigned long next_heartbeat = 0;
static inline void zf_set_status(unsigned char new)
{
 { outb(0x12, 0x218); outb(new, 0x219); };
}




static inline unsigned short zf_get_control(void)
{
 return zf_readw(0x10);
}

static inline void zf_set_control(unsigned short new)
{
 { outb(0x10, 0x218); outw(new, 0x21A); };
}







static inline void zf_set_timer(unsigned short new, unsigned char n)
{
 switch(n){
  case 0:
   { outb(0x0C, 0x218); outw(new, 0x21A); };
  case 1:
   { outb(0x0E, 0x218); outb(new > 0xff ? 0xff : new, 0x219); };
  default:
   return;
 }
}




static void zf_timer_off(void)
{
 unsigned int ctrl_reg = 0;
 unsigned long flags;


 del_timer(&zf_timer);

 spin_lock_irqsave(&zf_port_lock, flags);

 ctrl_reg = zf_get_control();
 ctrl_reg |= (0x0001|0x0002);
 ctrl_reg &= ~(0x0001|0x0002);
 zf_set_control(ctrl_reg);
 spin_unlock_irqrestore(&zf_port_lock, flags);

 printk("<6>" "machzwd" ": Watchdog timer is now disabled\n");
}





static void zf_timer_on(void)
{
 unsigned int ctrl_reg = 0;
 unsigned long flags;

 spin_lock_irqsave(&zf_port_lock, flags);

 { outb(0x0F, 0x218); outb(0xff, 0x219); };

 zf_set_timer(0xffff, 0);


 next_heartbeat = jiffies + (100*10);


 zf_timer.expires = jiffies + (100/2);

 add_timer(&zf_timer);


 ctrl_reg = zf_get_control();
 ctrl_reg |= (0x0001|zf_action);
 zf_set_control(ctrl_reg);
 spin_unlock_irqrestore(&zf_port_lock, flags);

 printk("<6>" "machzwd" ": Watchdog timer is now enabled\n");
}


static void zf_ping(unsigned long data)
{
 unsigned int ctrl_reg = 0;
 unsigned long flags;

 { outb(0x0E, 0x218); outb(0xff, 0x219); };

 if(((long)(jiffies) - (long)(next_heartbeat) < 0)){

  ;






  spin_lock_irqsave(&zf_port_lock, flags);
  ctrl_reg = zf_get_control();
  ctrl_reg |= 0x0010;
  zf_set_control(ctrl_reg);


  ctrl_reg &= ~(0x0010);
  zf_set_control(ctrl_reg);
  spin_unlock_irqrestore(&zf_port_lock, flags);

  zf_timer.expires = jiffies + (100/2);
  add_timer(&zf_timer);
 }else{
  printk("<2>" "machzwd" ": I will reset your machine\n");
 }
}

static ssize_t zf_write(struct file *file, const char *buf, size_t count,
        loff_t *ppos)
{

 if(count){





  if (!nowayout) {
   size_t ofs;





   zf_expect_close = 0;


   for (ofs = 0; ofs != count; ofs++){
    char c;
    if (get_user(c, buf + ofs))
     return -14;
    if (c == 'V'){
     zf_expect_close = 42;
     ;
    }
   }
  }





  next_heartbeat = jiffies + (100*10);
  ;

 }

 return count;
}

static int zf_ioctl(struct inode *inode, struct file *file, unsigned int cmd,
 unsigned long arg)
{
 void *argp = (void *)arg;
 int *p = argp;
 switch(cmd){
  case (((2U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((0)) << 0) | (((((sizeof(struct watchdog_info) == sizeof(struct watchdog_info[1]) && sizeof(struct watchdog_info) < (1 << 14)) ? sizeof(struct watchdog_info) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))):
   if (copy_to_user(argp, &zf_info, sizeof(zf_info)))
    return -14;
   break;

  case (((2U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((1)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))):
   return put_user(0, p);

  case (((2U) << (((0 +8)+8)+14)) | ((('W')) << (0 +8)) | (((5)) << 0) | (((((sizeof(int) == sizeof(int[1]) && sizeof(int) < (1 << 14)) ? sizeof(int) : __invalid_size_argument_for_IOC))) << ((0 +8)+8))):
   zf_ping(0);
   break;

  default:
   return -25;
 }

 return 0;
}

static int zf_open(struct inode *inode, struct file *file)
{
 spin_lock(&zf_lock);
 if(test_and_set_bit(0, &zf_is_open)) {
  spin_unlock(&zf_lock);
  return -16;
 }

 if (nowayout)
  __module_get(((struct module *) 0));

 spin_unlock(&zf_lock);

 zf_timer_on();

 return nonseekable_open(inode, file);
}

static int zf_close(struct inode *inode, struct file *file)
{
 if(zf_expect_close == 42){
  zf_timer_off();
 } else {
  del_timer(&zf_timer);
  printk("<3>" "machzwd" ": device file closed unexpectedly. Will not stop the WDT!\n");
 }

 spin_lock(&zf_lock);
 clear_bit(0, &zf_is_open);
 spin_unlock(&zf_lock);

 zf_expect_close = 0;

 return 0;
}





static int zf_notify_sys(struct notifier_block *this, unsigned long code,
        void *unused)
{
 if(code == 0x0001 || code == 0x0002){
  zf_timer_off();
 }

 return 0x0000;
}




static const struct file_operations zf_fops = {
 .owner = ((struct module *) 0),
 .llseek = no_llseek,
 .write = zf_write,
 .ioctl = zf_ioctl,
 .open = zf_open,
 .release = zf_close,
};

static struct miscdevice zf_miscdev = {
 .minor = 130,
 .name = "watchdog",
 .fops = (struct file_operations *)&zf_fops,
};






static struct notifier_block zf_notifier = {
 .notifier_call = zf_notify_sys,
};

static void zf_show_action(int act)
{
 char *str[] = { "RESET", "SMI", "NMI", "SCI" };

 printk("<6>" "machzwd" ": Watchdog using action = %s\n", str[act]);
}

static int zf_init(void)
{
 int ret;

 printk("<6>" "machzwd" ": MachZ ZF-Logic Watchdog driver initializing.\n");

 ret = zf_readw(0x02);
 if ((!ret) || (ret == 0xffff)) {
  printk("<4>" "machzwd" ": no ZF-Logic found\n");
  return -19;
 }

 if((action <= 3) && (action >= 0)){
  zf_action = zf_action>>action;
 } else
  action = 0;

 zf_show_action(action);

 spin_lock_init(&zf_lock);
 spin_lock_init(&zf_port_lock);

 ret = misc_register(&zf_miscdev);
 if (ret){
  printk("<3>" "can't misc_register on minor=%d\n",
       130);
  goto out;
 }

 if(!request_region(0x218, 3, "MachZ ZFL WDT")){
  printk("<3>" "cannot reserve I/O ports at %d\n",
       0x218);
  ret = -16;
  goto no_region;
 }

 ret = register_reboot_notifier(&zf_notifier);
 if(ret){
  printk("<3>" "can't register reboot notifier (err=%d)\n",
         ret);
  goto no_reboot;
 }

 zf_set_status(0);
 zf_set_control(0);


 init_timer(&zf_timer);
 zf_timer.function = zf_ping;
 zf_timer.data = 0;

 return 0;

no_reboot:
 release_region(0x218, 3);
no_region:
 misc_deregister(&zf_miscdev);
out:
 return ret;
}


static void zf_exit(void)
{
 zf_timer_off();

 misc_deregister(&zf_miscdev);
 unregister_reboot_notifier(&zf_notifier);
 release_region(0x218, 3);
}

int (* _ddv_tmp_init)(void) = zf_init;
void (* _ddv_tmp_exit)(void) = zf_exit;

int main()
{
   _ddv_module_init = zf_init;
   _ddv_module_exit = zf_exit;
   call_ddv();

   return 0;
}










struct cdev {
  struct module *owner;
  struct file_operations *ops;
  dev_t dev;
  unsigned int count;
};

void cdev_init(struct cdev *, struct file_operations *);

struct cdev *cdev_alloc(void);

void cdev_put(struct cdev *p);

int cdev_add(struct cdev *, dev_t, unsigned);

void cdev_del(struct cdev *);

void cd_forget(struct inode *);













struct cdev fixed_cdev[1];
int fixed_cdev_used = 0;

short number_cdev_registered = 0;

struct ddv_cdev {
    struct cdev *cdevp;

    struct file filp;
    struct inode inode;

    int open;
};

struct ddv_cdev cdev_registered[1];

void call_cdev_functions();



















struct kobject;
struct module;

struct attribute {
 const char * name;
 struct module * owner;
 mode_t mode;
};

struct attribute_group {
 const char * name;
 struct attribute ** attrs;
};
struct kobject {
  int something;
};











typedef struct pm_message {
 int event;
} pm_message_t;



struct device {
    void *driver_data;
    void (*release)(struct device * dev);
};

struct device_driver {
 const char * name;
};


struct class {
  int someting;
};

struct class_device {
  int something;
};

struct class_interface {
  int something;
};

struct class_device_attribute {
 struct attribute attr;
 ssize_t (*show)(struct class_device *, char * buf);
 ssize_t (*store)(struct class_device *, const char * buf, size_t count);
};





extern struct class *class_create(struct module *owner, const char *name);
extern void class_destroy(struct class *cls);

extern struct class_device *class_device_create(struct class *cls,
      struct class_device *parent,
      dev_t devt,
      struct device *device,
      const char *fmt, ...);
extern void class_device_destroy(struct class *cls, dev_t devt);


struct device_attribute {
 struct attribute attr;
 ssize_t (*show)(struct device *dev, struct device_attribute *attr,
   char *buf);
 ssize_t (*store)(struct device *dev, struct device_attribute *attr,
    const char *buf, size_t count);
};





static inline void * dev_get_drvdata (struct device *dev)
{
 return dev->driver_data;
}

static inline void dev_set_drvdata (struct device *dev, void *data)
{
 dev->driver_data = data;
}

extern const char *dev_driver_string(struct device *dev);
struct gendisk {
 int major;
 int first_minor;
 int minors;

 char disk_name[32];
 struct block_device_operations *fops;
 struct request_queue *queue;
 void *private_data;

 int flags;
 struct device *driverfs_dev;
 char devfs_name[64];
};


void add_disk(struct gendisk *disk);

void del_gendisk(struct gendisk *gp);

struct gendisk *alloc_disk(int minors);

void put_disk(struct gendisk *disk);

extern struct kobject *get_disk(struct gendisk *disk);


void set_capacity(struct gendisk *disk, sector_t size);


void add_disk_randomness(struct gendisk *disk);


struct work_struct {
    unsigned long pending;
    void (*func)(void *);
    void *data;

    int init;
};
int schedule_work(struct work_struct *work);

void flush_scheduled_work(void);






struct mutex {
    int locked;
    int init;
};
void mutex_init(struct mutex *lock);

void mutex_lock(struct mutex *lock);

void mutex_unlock(struct mutex *lock);
struct page {
    int something;
};



void *page_address(struct page *page);




















struct pt_regs {
    int something;
};
typedef int irqreturn_t;





struct tasklet_struct
{
    atomic_t count;
    void (*func)(unsigned long);
    unsigned long data;

    int init;
};
void tasklet_schedule(struct tasklet_struct *t);

static inline void tasklet_disable(struct tasklet_struct *t)
{
    t->count++;
}

static inline void tasklet_enable(struct tasklet_struct *t)
{
    t->count++;
}


void tasklet_kill(struct tasklet_struct *t);

void tasklet_init(struct tasklet_struct *t,
    void (*func)(unsigned long), unsigned long data);




typedef irqreturn_t (*irq_handler_t)(int, void *, struct pt_regs *);



int request_irq(unsigned int, irq_handler_t handler,
         unsigned long, const char *, void *);

void free_irq(unsigned int, void *);


unsigned long probe_irq_on(void);

int probe_irq_off(unsigned long);

unsigned int probe_irq_mask(unsigned long);


void cli(void);

void sti(void);

void save_flags(unsigned long x);

void restore_flags(unsigned long x);




enum km_type {
 KM_BOUNCE_READ,
 KM_SKB_SUNRPC_DATA,
 KM_SKB_DATA_SOFTIRQ,
 KM_USER0,
 KM_USER1,
 KM_BIO_SRC_IRQ,
 KM_BIO_DST_IRQ,
 KM_PTE0,
 KM_PTE1,
 KM_IRQ0,
 KM_IRQ1,
 KM_SOFTIRQ0,
 KM_SOFTIRQ1,
 KM_KDB,
 KM_DUMP,
 KM_TYPE_NR
};



void *kmap(struct page *page);
void kunmap(struct page *page);
void *kmap_atomic(struct page *page, enum km_type type);
void kunmap_atomic(void *kvaddr, enum km_type type);
void *kmap_atomic_pfn(unsigned long pfn, enum km_type type);
struct page *kmap_atomic_to_page(void *ptr);
struct backing_dev_info {
    unsigned long ra_pages;
    unsigned long state;
    unsigned int capabilities;
};







struct bio_vec {
    struct page *bv_page;
    unsigned int bv_len;
    unsigned int bv_offset;
};

struct bio_set;
struct bio;
typedef int (bio_end_io_t) (struct bio *, unsigned int, int);
typedef void (bio_destructor_t) (struct bio *);

struct bio {
    sector_t bi_sector;

    struct bio *bi_next;
    struct block_device *bi_bdev;
    unsigned long bi_flags;
    unsigned long bi_rw;

    unsigned short bi_vcnt;
    unsigned short bi_idx;




    unsigned short bi_phys_segments;

    unsigned int bi_size;
    struct bio_vec *bi_io_vec;

    bio_end_io_t *bi_end_io;

    void *bi_private;
};
void * __bio_kmap_atomic(struct bio *, int, enum km_type);
void __bio_kunmap_atomic(char *buffer, enum km_type);

void bio_endio(struct bio *, unsigned int, int);

int bio_cur_sectors(struct bio *);






struct scatterlist {
    struct page *page;
    unsigned int offset;
    dma_addr_t dma_address;
    unsigned int length;
};




struct request *elv_next_request(struct request_queue *q);

struct request_queue;
typedef struct request_queue request_queue_t;

typedef void (request_fn_proc) (request_queue_t *q);
typedef int (make_request_fn) (request_queue_t *q, struct bio *bio);
typedef void (unplug_fn) (request_queue_t *);




enum rq_cmd_type_bits {
 REQ_TYPE_FS = 1,
 REQ_TYPE_BLOCK_PC,
 REQ_TYPE_SENSE,
 REQ_TYPE_PM_SUSPEND,
 REQ_TYPE_PM_RESUME,
 REQ_TYPE_PM_SHUTDOWN,
 REQ_TYPE_FLUSH,
 REQ_TYPE_SPECIAL,
 REQ_TYPE_LINUX_BLOCK,





 REQ_TYPE_ATA_CMD,
 REQ_TYPE_ATA_TASK,
 REQ_TYPE_ATA_TASKFILE,
 REQ_TYPE_ATA_PC,
};


struct request_queue {
    request_fn_proc *request_fn;
    make_request_fn *make_request_fn;
    unplug_fn *unplug_fn;

    struct backing_dev_info backing_dev_info;
    void *queuedata;




    unsigned long queue_flags;

    spinlock_t *queue_lock;

    unsigned short hardsect_size;

    int __ddv_genhd_no;
    int __ddv_queue_alive;
};
struct request {
    struct list_head queuelist;
    struct list_head donelist;

    request_queue_t *q;


    unsigned long flags;

    unsigned int cmd_flags;
    enum rq_cmd_type_bits cmd_type;

    struct bio *bio;

    void *completion_data;

    struct gendisk *rq_disk;
    sector_t sector;
    unsigned long nr_sectors;
    unsigned int current_nr_sectors;
    char *buffer;

    int errors;

    unsigned short nr_phys_segments;

    unsigned char cmd[16];
};






enum rq_flag_bits {
 __REQ_RW,
 __REQ_FAILFAST,
 __REQ_SORTED,
 __REQ_SOFTBARRIER,
 __REQ_HARDBARRIER,
 __REQ_FUA,
 __REQ_CMD,
 __REQ_NOMERGE,
 __REQ_STARTED,
 __REQ_DONTPREP,
 __REQ_QUEUED,
 __REQ_ELVPRIV,



 __REQ_PC,
 __REQ_BLOCK_PC,
 __REQ_SENSE,

 __REQ_FAILED,
 __REQ_QUIET,
 __REQ_SPECIAL,
 __REQ_DRIVE_CMD,
 __REQ_DRIVE_TASK,
 __REQ_DRIVE_TASKFILE,
 __REQ_PREEMPT,
 __REQ_PM_SUSPEND,
 __REQ_PM_RESUME,
 __REQ_PM_SHUTDOWN,
 __REQ_ORDERED_COLOR,
 __REQ_NR_BITS,
};
request_queue_t *blk_alloc_queue(gfp_t);

request_queue_t *blk_init_queue(request_fn_proc *, spinlock_t *);

void blk_queue_make_request(request_queue_t *, make_request_fn *);

void blk_queue_hardsect_size(request_queue_t *, unsigned short);

void blk_cleanup_queue(request_queue_t *);

void blk_put_queue(request_queue_t *);



void blk_plug_device(request_queue_t *);

int blk_remove_plug(request_queue_t *);


void blkdev_dequeue_request(struct request *req);


int end_that_request_first(struct request *, int, int);

int end_that_request_chunk(struct request *, int, int);

void end_that_request_last(struct request *, int);

void end_request(struct request *req, int uptodate);

void blk_complete_request(struct request *);


void blk_queue_bounce_limit(request_queue_t *, u64);


void blk_queue_max_phys_segments(request_queue_t *, unsigned short);

void blk_queue_max_hw_segments(request_queue_t *, unsigned short);


int blk_rq_map_sg(request_queue_t *, struct request *, struct scatterlist *);







short number_genhd_registered = 0;
short number_fixed_genhd_used = 0;

struct ddv_genhd {
  struct gendisk *gd;

  struct inode inode;
  struct file file;
  struct request current_request;
  int requests_open;
};

struct gendisk fixed_gendisk[10];
struct ddv_genhd genhd_registered[10];

void call_genhd_functions();
















typedef unsigned long kernel_ulong_t;



struct pci_device_id {
 __u32 vendor, device;
 __u32 subvendor, subdevice;
 __u32 class, class_mask;
 kernel_ulong_t driver_data;
};




typedef int pci_power_t;


struct pci_dev {
    struct pci_bus *bus;

    unsigned int devfn;
    unsigned short vendor;
    unsigned short device;


    u64 dma_mask;

    struct device dev;

    unsigned int irq;
    struct resource resource[12];
};




struct pci_bus {
    unsigned char number;
};


struct pci_driver {
    char *name;
    const struct pci_device_id *id_table;
    int (*probe) (struct pci_dev *dev, const struct pci_device_id *id);
    void (*remove) (struct pci_dev *dev);
    int (*suspend) (struct pci_dev *dev, pm_message_t state);
    int (*resume) (struct pci_dev *dev);
    int (*enable_wake) (struct pci_dev *dev, pci_power_t state, int enable);
    void (*shutdown) (struct pci_dev *dev);
};







int pci_dev_present(const struct pci_device_id *ids);


struct pci_dev *pci_get_class (unsigned int class, struct pci_dev *from);

int pci_set_dma_mask(struct pci_dev *dev, u64 mask);
void pci_set_master(struct pci_dev *dev);



int pci_register_driver(struct pci_driver *);

void pci_unregister_driver(struct pci_driver *);

int pci_enable_device(struct pci_dev *dev);

void pci_disable_device(struct pci_dev *dev);




int pci_request_regions(struct pci_dev *, const char *);

void pci_release_regions(struct pci_dev *);

int pci_request_region(struct pci_dev *, int, const char *);

void pci_release_region(struct pci_dev *, int);








void * pci_alloc_consistent(struct pci_dev *hwdev, size_t size,
       dma_addr_t *dma_handle);

void pci_free_consistent(struct pci_dev *hwdev, size_t size,
    void *vaddr, dma_addr_t dma_handle);

dma_addr_t pci_map_single(struct pci_dev *hwdev, void *ptr, size_t size, int direction);
void pci_unmap_single(struct pci_dev *hwdev, dma_addr_t dma_addr, size_t size, int direction);

dma_addr_t pci_map_page(struct pci_dev *hwdev, struct page *page,
   unsigned long offset, size_t size, int direction);
void pci_unmap_page(struct pci_dev *hwdev, dma_addr_t dma_address,
      size_t size, int direction);

struct ddv_pci_driver {
    struct pci_driver *pci_driver;
    struct pci_dev pci_dev;

    unsigned int no_pci_device_id;
    int dev_initialized;
};

struct ddv_pci_driver registered_pci_driver;

int pci_probe_device();
void pci_remove_device();

void call_pci_functions();
struct registered_irq {
  irq_handler_t handler;
  void *dev_id;
};

struct registered_irq registered_irq[16];

void call_interrupt_handler();







short number_tasklet_registered = 0;

struct ddv_tasklet {
  struct tasklet_struct *tasklet;
  unsigned short is_running;
};

struct ddv_tasklet tasklet_registered[1];

void call_tasklet_functions();







short number_timer_registered = 0;

struct ddv_timer {
  struct timer_list *timer;
};

struct ddv_timer timer_registered[1];

void call_timer_functions();







struct work_struct *shared_workqueue[10];

void call_shared_workqueue_functions();







spinlock_t kernel_lock;

void init_kernel()
{
    int i;

    spin_lock_init(&kernel_lock);

    for (i = 0; i < 10; i++) {
 shared_workqueue[i] = ((void *)0);
    }

    for (i = 0; i < 1; i++) {
 tasklet_registered[i].tasklet = ((void *)0);
 tasklet_registered[i].is_running = 0;
    }
}

void ddv()
{
    unsigned short random;

    do {
 random = __VERIFIER_nondet_ushort();

 switch (random) {
     case 1:
  current_execution_context = 1;

  call_cdev_functions();





  break;

     case 2:
  current_execution_context = 2;
  call_timer_functions();
  break;

     case 3:
  current_execution_context = 2;
  call_interrupt_handler();
  break;

     case 4:
  current_execution_context = 1;
  call_shared_workqueue_functions();
  break;

     case 5:
  current_execution_context = 2;
  call_tasklet_functions();
  break;
     default:
  break;
 }
    } while(random);
}

int call_ddv()
{
    int err;

    current_execution_context = 1;

    init_kernel();

    err = (* _ddv_module_init)();

    if (err) {
 return -1;
    }

    ddv();

    current_execution_context = 1;
    (* _ddv_module_exit)();

    return 0;
}




void call_cdev_functions()
{
    int cdev_no, function_no, result;

    loff_t loff_t_value;
    int int_value;
    unsigned int uint_value;
    unsigned long ulong_value;
    char char_value;
    size_t size_t_value;

    if (number_cdev_registered == 0) {
 return;
    }

    cdev_no = __VERIFIER_nondet_ushort();
    assume_abort_if_not (0 <= cdev_no && cdev_no < number_cdev_registered);

    switch (__VERIFIER_nondet_ushort()) {
 case 0:
     if (cdev_registered[cdev_no].cdevp->ops->llseek) {
  loff_t_value = __VERIFIER_nondet_loff_t();
  int_value = __VERIFIER_nondet_int();

  (* cdev_registered[cdev_no].cdevp->ops->llseek)(&cdev_registered[cdev_no].filp,
        loff_t_value,
        int_value);
     }
     break;
 case 1:
     if (cdev_registered[cdev_no].cdevp->ops->read) {
  char_value = __VERIFIER_nondet_char();
  size_t_value = __VERIFIER_nondet_size_t();

  (* cdev_registered[cdev_no].cdevp->ops->read)(&cdev_registered[cdev_no].filp,
             &char_value,
             size_t_value,
             &loff_t_value);
     }
     break;
 case 2:

     break;
 case 3:
     if (cdev_registered[cdev_no].cdevp->ops->write) {
  char_value = __VERIFIER_nondet_char();
  size_t_value = __VERIFIER_nondet_size_t();

  (* cdev_registered[cdev_no].cdevp->ops->write)(&cdev_registered[cdev_no].filp,
              &char_value,
              size_t_value,
              &loff_t_value);
     }
     break;
 case 4:

     break;
 case 5:

     break;
 case 6:

     break;
 case 7:
     if (cdev_registered[cdev_no].cdevp->ops->ioctl) {
  uint_value = __VERIFIER_nondet_uint();
  ulong_value = __VERIFIER_nondet_ulong();

  (* cdev_registered[cdev_no].cdevp->ops->ioctl)(&cdev_registered[cdev_no].inode,
              &cdev_registered[cdev_no].filp,
              uint_value,
              ulong_value);
     }

     break;
 case 8:

     break;
 case 9:

     break;
 case 10:

     break;
 case 11:
     if ((cdev_registered[cdev_no].cdevp->ops->open) &&
  (!cdev_registered[cdev_no].open)) {
  result = (* cdev_registered[cdev_no].cdevp->ops->open)(&cdev_registered[cdev_no].inode,
               &cdev_registered[cdev_no].filp);

  if (!result) {
      cdev_registered[cdev_no].open = 1;
  }
     }
     break;
 case 12:

     break;
 case 13:
     if ((cdev_registered[cdev_no].cdevp->ops->release) &&
         (cdev_registered[cdev_no].open)) {
  result = (* cdev_registered[cdev_no].cdevp->ops->release)(&cdev_registered[cdev_no].inode,
           &cdev_registered[cdev_no].filp);

  if (!result) {
      cdev_registered[cdev_no].open = 0;
  }
     }
     break;
 case 14:

     break;
 case 15:

     break;
 case 16:

     break;
 case 17:

     break;
 case 18:

     break;
 case 19:

     break;
 case 20:

     break;
 case 21:

     break;
 case 22:

     break;
 case 23:

     break;
 case 24:

     break;
 case 25:

     break;
 case 26:

     break;
 default:
     break;
    }
}




struct hd_geometry {
      unsigned char heads;
      unsigned char sectors;
      unsigned short cylinders;
      unsigned long start;
};




void
create_request(int genhd_no)
{
    struct request rq;

    rq.cmd_type = REQ_TYPE_FS;
    rq.rq_disk = genhd_registered[genhd_no].gd;
    rq.sector = __VERIFIER_nondet_sector_t();
    rq.current_nr_sectors = __VERIFIER_nondet_uint();
    rq.buffer = __VERIFIER_nondet_pchar();

    genhd_registered[genhd_no].current_request = rq;
    genhd_registered[genhd_no].requests_open = 1;
}

void call_rq_function(int genhd_no)
{
    if ((genhd_registered[genhd_no].gd->queue->request_fn != ((void *)0)) &&
 (genhd_registered[genhd_no].gd->queue->__ddv_queue_alive))
    {
 spin_lock(genhd_registered[genhd_no].gd->queue->queue_lock);

 create_request(genhd_no);
 genhd_registered[genhd_no].gd->queue->__ddv_genhd_no = genhd_no;

 (* genhd_registered[genhd_no].gd->queue->request_fn)(genhd_registered[genhd_no].gd->queue);


 spin_unlock(genhd_registered[genhd_no].gd->queue->queue_lock);

 return;
    }

    if (genhd_registered[genhd_no].gd->queue->make_request_fn) {
 return;
    }
}

void call_genhd_functions()
{
    unsigned short genhd_no, function_no;
    unsigned int uint_value;
    unsigned long ulong_value;
    int result;

    if (number_genhd_registered == 0) {
 return;
    }

    genhd_no = __VERIFIER_nondet_ushort();
    assume_abort_if_not (genhd_no < number_genhd_registered);


    function_no = __VERIFIER_nondet_ushort();

    switch (function_no) {
 case 0:
     call_rq_function(genhd_no);
     break;

 case 1:
     if (genhd_registered[genhd_no].gd->fops->open) {
  genhd_registered[genhd_no].inode.i_bdev = (struct block_device*)malloc(sizeof(struct block_device));
  genhd_registered[genhd_no].inode.i_bdev->bd_disk = genhd_registered[genhd_no].gd;

  (* genhd_registered[genhd_no].gd->fops->open)(&genhd_registered[genhd_no].inode,
             &genhd_registered[genhd_no].file);
     }
     break;

 case 2:
     if (genhd_registered[genhd_no].gd->fops->release) {
  (* genhd_registered[genhd_no].gd->fops->release)(&genhd_registered[genhd_no].inode,
         &genhd_registered[genhd_no].file);
     }
     break;

 case 3:
     if (genhd_registered[genhd_no].gd->fops->ioctl) {
  uint_value = __VERIFIER_nondet_uint();
  ulong_value = __VERIFIER_nondet_ulong();
  (* genhd_registered[genhd_no].gd->fops->ioctl)(&genhd_registered[genhd_no].inode,
              &genhd_registered[genhd_no].file,
              uint_value,
              ulong_value);
     }
     break;

 case 4:
     if (genhd_registered[genhd_no].gd->fops->media_changed) {
  (* genhd_registered[genhd_no].gd->fops->media_changed)(genhd_registered[genhd_no].gd);
     }
     break;

 case 5:
     if (genhd_registered[genhd_no].gd->fops->revalidate_disk) {
  (* genhd_registered[genhd_no].gd->fops->revalidate_disk)(genhd_registered[genhd_no].gd);
     }
     break;

 case 6:
     if (genhd_registered[genhd_no].gd->fops->getgeo) {
  struct hd_geometry hdg;
  struct block_device blk_dev;

  blk_dev.bd_disk = genhd_registered[genhd_no].gd;

  (* genhd_registered[genhd_no].gd->fops->getgeo)(&blk_dev, &hdg);
     }
     break;

 default:
     break;
    }
}
void call_interrupt_handler()
{
    unsigned short i;
    struct pt_regs regs;

    i = __VERIFIER_nondet_int();
    assume_abort_if_not(i < 16);

    if (registered_irq[i].handler) {
      (* registered_irq[i].handler)((int)i, registered_irq[i].dev_id, &regs)
                                                  ;
    }
}
void create_pci_dev()
{
}

int pci_probe_device()
{
    int err;
    unsigned int dev_id;

    registered_pci_driver.no_pci_device_id = 1;

    dev_id = __VERIFIER_nondet_uint();
    assume_abort_if_not(dev_id < registered_pci_driver.no_pci_device_id);

    err = (*registered_pci_driver.pci_driver->probe)(&registered_pci_driver.pci_dev,
           &registered_pci_driver.pci_driver->id_table[dev_id]);

    if (!err) {
 registered_pci_driver.dev_initialized = 1;
    }

    return err;
}

void pci_remove_device()
{
    (*registered_pci_driver.pci_driver->remove)(&registered_pci_driver.pci_dev);

    registered_pci_driver.dev_initialized = 0;
}

void call_pci_functions()
{
    switch (__VERIFIER_nondet_uint()) {
 case 0:
     if (!registered_pci_driver.dev_initialized) {
  pci_probe_device();
     }
     break;

 case 1:
     if (registered_pci_driver.dev_initialized) {
  pci_remove_device();
     }
     break;

 default:
     break;
    }
}


void call_tasklet_functions()
{
  unsigned int i;
  assume_abort_if_not(i < 1);

  if ((tasklet_registered[i].tasklet != ((void *)0)) &&
      (tasklet_registered[i].tasklet->count == 0)) {
    tasklet_registered[i].is_running = 1;
    (* tasklet_registered[i].tasklet->func)(tasklet_registered[i].tasklet->data);
    tasklet_registered[i].is_running = 0;
    tasklet_registered[i].tasklet = ((void *)0);
  }
}



void call_timer_functions()
{
  unsigned short i = __VERIFIER_nondet_ushort();

  assume_abort_if_not(i < number_timer_registered);

  if (timer_registered[i].timer->__ddv_active) {
    (* timer_registered[i].timer->function)(timer_registered[i].timer->data);
  }
}







inline int pci_enable_device(struct pci_dev *dev)
{
    int i;

    for (i = 0; i < 12; i++) {
 dev->resource[i].flags = 0x00000100;
 dev->resource[i].start = __VERIFIER_nondet_uint();
 dev->resource[i].end = dev->resource[i].start + __VERIFIER_nondet_ushort();
    }
}

inline struct pci_dev *pci_get_class (unsigned int class, struct pci_dev *from)
{
    if (from == ((void *)0)) {
 from = (struct pci_dev*)malloc(sizeof(struct pci_dev));
    }

    if (__VERIFIER_nondet_int()) {
 from->vendor = __VERIFIER_nondet_ushort();
 from->device = __VERIFIER_nondet_ushort();
 from->irq = __VERIFIER_nondet_uint();
 assume_abort_if_not(from->irq < 16);

 return from;
    } else {
 return ((void *)0);
    }
}

inline int pci_register_driver(struct pci_driver *driver)
{
    if (__VERIFIER_nondet_int()) {
 registered_pci_driver.pci_driver = driver;
 registered_pci_driver.no_pci_device_id = sizeof(driver->id_table) / sizeof(struct pci_device_id);
 registered_pci_driver.dev_initialized = 0;

 return 0;
    } else {
 return -1;
    }
}

inline void pci_unregister_driver(struct pci_driver *driver)
{
    registered_pci_driver.pci_driver = ((void *)0);
    registered_pci_driver.no_pci_device_id = 0;
}

inline void pci_release_region(struct pci_dev *pdev, int bar)
{
    if ((((((pdev))->resource[((bar))].start) == 0 && (((pdev))->resource[((bar))].end) == (((pdev))->resource[((bar))].start)) ? 0 : ((((pdev))->resource[((bar))].end) - (((pdev))->resource[((bar))].start) + 1)) == 0)
 return;
    if (((pdev)->resource[(bar)].flags) & 0x00000100)
 release_region(((pdev)->resource[(bar)].start),
         (((((pdev))->resource[((bar))].start) == 0 && (((pdev))->resource[((bar))].end) == (((pdev))->resource[((bar))].start)) ? 0 : ((((pdev))->resource[((bar))].end) - (((pdev))->resource[((bar))].start) + 1)));
    else if (((pdev)->resource[(bar)].flags) & 0x00000200)
 release_mem_region(((pdev)->resource[(bar)].start),
      (((((pdev))->resource[((bar))].start) == 0 && (((pdev))->resource[((bar))].end) == (((pdev))->resource[((bar))].start)) ? 0 : ((((pdev))->resource[((bar))].end) - (((pdev))->resource[((bar))].start) + 1)));
}

inline int pci_request_region(struct pci_dev *pdev, int bar, const char *res_name)
{
    if ((((((pdev))->resource[((bar))].start) == 0 && (((pdev))->resource[((bar))].end) == (((pdev))->resource[((bar))].start)) ? 0 : ((((pdev))->resource[((bar))].end) - (((pdev))->resource[((bar))].start) + 1)) == 0)
 return 0;

    if (((pdev)->resource[(bar)].flags) & 0x00000100) {
 if (!request_region(((pdev)->resource[(bar)].start),
       (((((pdev))->resource[((bar))].start) == 0 && (((pdev))->resource[((bar))].end) == (((pdev))->resource[((bar))].start)) ? 0 : ((((pdev))->resource[((bar))].end) - (((pdev))->resource[((bar))].start) + 1)), res_name))
     return -16;
    }
    else if (((pdev)->resource[(bar)].flags) & 0x00000200) {
 if (!request_mem_region(((pdev)->resource[(bar)].start),
    (((((pdev))->resource[((bar))].start) == 0 && (((pdev))->resource[((bar))].end) == (((pdev))->resource[((bar))].start)) ? 0 : ((((pdev))->resource[((bar))].end) - (((pdev))->resource[((bar))].start) + 1)), res_name))
     return -16;
    }

    return 0;
}

inline void pci_release_regions(struct pci_dev *pdev)
{
    int i;

    for (i = 0; i < 6; i++)
 pci_release_region(pdev, i);
}

inline int pci_request_regions(struct pci_dev *pdev, const char *res_name)
{
    int i;

    for (i = 0; i < 6; i++)
 if(pci_request_region(pdev, i, res_name))
     goto err_out;
    return 0;

 err_out:
    while(--i >= 0)
 pci_release_region(pdev, i);

    return -16;
}




inline int __get_user(int size, void *ptr)
{
 __VERIFIER_HIDE:
    assert_context_process();

    return __VERIFIER_nondet_int();
}

inline int get_user(int size, const void *ptr)
{
 __VERIFIER_HIDE:
    assert_context_process();

    return __VERIFIER_nondet_int();
}

inline int __put_user(int size, void *ptr)
{
 __VERIFIER_HIDE:
    assert_context_process();

    return __VERIFIER_nondet_int();
}

inline int put_user(int size, void *ptr)
{
 __VERIFIER_HIDE:
    assert_context_process();

    return __VERIFIER_nondet_int();
}

inline unsigned long copy_to_user(void *to, const void *from, unsigned long n)
{
 __VERIFIER_HIDE:
    assert_context_process();

    return __VERIFIER_nondet_ulong();
}

inline unsigned long copy_from_user(void *to, void *from, unsigned long n)
{
 __VERIFIER_HIDE:
    assert_context_process();

   return __VERIFIER_nondet_ulong();
}





int register_blkdev(unsigned int major, const char *name)
{
  int result = __VERIFIER_nondet_int();





  return result;
}

int unregister_blkdev(unsigned int major, const char *name)
{
  return 0;
}

struct gendisk *alloc_disk(int minors)
{
  struct gendisk * gd;

  if (number_fixed_genhd_used < 10) {
    gd = &fixed_gendisk[number_fixed_genhd_used];
    gd->minors = minors;

    number_fixed_genhd_used++;

    return gd;
  } else {
    return ((void *)0);
  }
}

void add_disk(struct gendisk *disk)
{
  if (number_genhd_registered < 10) {
    genhd_registered[number_genhd_registered].gd = disk;
    genhd_registered[number_genhd_registered].inode.i_bdev = (struct block_device*)malloc(sizeof(struct block_device));
    genhd_registered[number_genhd_registered].inode.i_bdev->bd_disk = disk;

    number_genhd_registered++;
  }
}

void del_gendisk(struct gendisk *gp)
{
  int i;

  for (i = 0; i < number_genhd_registered; i++) {
    if (genhd_registered[i].gd == gp) {
      genhd_registered[i].gd = ((void *)0);
    }
  }
}









request_queue_t fixed_request_queue[10];

int number_request_queue_used = 0;

request_queue_t *get_fixed_request_queue()
{
    if (number_request_queue_used < 10) {
 return &fixed_request_queue[number_request_queue_used++];
    } else {
 return ((void *)0);
    }
}

request_queue_t *blk_init_queue(request_fn_proc *rfn, spinlock_t *lock)
{
    request_queue_t *queue;

    if (__VERIFIER_nondet_int()) {
 queue = get_fixed_request_queue();

 queue->queue_lock = lock;
 queue->request_fn = rfn;
 queue->make_request_fn = ((void *)0);
 queue->__ddv_queue_alive = 1;

 return queue;
    } else {
 return ((void *)0);
    }
}

request_queue_t *blk_alloc_queue(gfp_t gfp_mask)
{
    request_queue_t *queue;

    if (__VERIFIER_nondet_int()) {
 queue = get_fixed_request_queue();

 queue->request_fn = ((void *)0);
 queue->make_request_fn = ((void *)0);
 queue->__ddv_queue_alive = 1;

 return queue;
    } else {
 return ((void *)0);
    }
}

void blk_queue_make_request(request_queue_t * q, make_request_fn * mfn)
{
  q->make_request_fn = mfn;
}

void end_request(struct request *req, int uptodate)
{
  int genhd_no = req->rq_disk->queue->__ddv_genhd_no;

  genhd_registered[genhd_no].requests_open = 0;
}


void blk_queue_hardsect_size(request_queue_t *q, unsigned short size)
{
  q->hardsect_size = size;
}

void blk_cleanup_queue(request_queue_t *q)
{
  q->__ddv_queue_alive = 0;
}












struct proc_dir_entry {
  int something;
};

struct proc_dir_entry *proc_root_driver;

typedef int (read_proc_t)(char *page, char **start, off_t off,
     int count, int *eof, void *data);

struct proc_dir_entry *create_proc_read_entry(const char *name,
           mode_t mode,
           struct proc_dir_entry *base,
           read_proc_t *read_proc,
           void * data);

struct proc_dir_entry *create_proc_entry(const char *name,
      mode_t mode,
      struct proc_dir_entry *parent);

void remove_proc_entry(const char *name, struct proc_dir_entry *parent);
struct proc_dir_entry *proc_mkdir(const char *,struct proc_dir_entry *);







struct seq_operations;
struct file;
struct vfsmount;
struct dentry;
struct inode;

struct seq_file {
    int something;
};

struct seq_operations {
    void * (*start) (struct seq_file *m, loff_t *pos);
    void (*stop) (struct seq_file *m, void *v);
    void * (*next) (struct seq_file *m, void *v, loff_t *pos);
    int (*show) (struct seq_file *m, void *v);
};

int seq_open(struct file *, struct seq_operations *);
ssize_t seq_read(struct file *, char *, size_t, loff_t *);
loff_t seq_lseek(struct file *, loff_t, int);
int seq_release(struct inode *, struct file *);
int seq_escape(struct seq_file *, const char *, const char *);
int seq_putc(struct seq_file *m, char c);
int seq_puts(struct seq_file *m, const char *s);

int seq_printf(struct seq_file *, const char *, ...);








int misc_register(struct miscdevice * misc)
{
  int i;
  dev_t dev;

  if (fixed_cdev_used < 1) {
    i = fixed_cdev_used;
    fixed_cdev_used++;

    fixed_cdev[i].owner = ((struct module *) 0);
    fixed_cdev[i].ops = misc->fops;

    dev = (((10) << 20) | (misc->minor));

    return cdev_add(&fixed_cdev[i], dev, 0);
  } else {
    return -1;
  }
}

















typedef unsigned char cc_t;
typedef unsigned int speed_t;
typedef unsigned int tcflag_t;


struct termios {
 tcflag_t c_iflag;
 tcflag_t c_oflag;
 tcflag_t c_cflag;
 tcflag_t c_lflag;
 cc_t c_line;
 cc_t c_cc[19];
};







struct termio {
 unsigned short c_iflag;
 unsigned short c_oflag;
 unsigned short c_cflag;
 unsigned short c_lflag;
 unsigned char c_line;
 unsigned char c_cc[8];
};

struct tty_struct;


struct tty_operations {
 int (*open)(struct tty_struct * tty, struct file * filp);
 void (*close)(struct tty_struct * tty, struct file * filp);
 int (*write)(struct tty_struct * tty,
        const unsigned char *buf, int count);
 void (*put_char)(struct tty_struct *tty, unsigned char ch);
 void (*flush_chars)(struct tty_struct *tty);
 int (*write_room)(struct tty_struct *tty);
 int (*chars_in_buffer)(struct tty_struct *tty);
 int (*ioctl)(struct tty_struct *tty, struct file * file,
      unsigned int cmd, unsigned long arg);
 void (*set_termios)(struct tty_struct *tty, struct termios * old);
 void (*throttle)(struct tty_struct * tty);
 void (*unthrottle)(struct tty_struct * tty);
 void (*stop)(struct tty_struct *tty);
 void (*start)(struct tty_struct *tty);
 void (*hangup)(struct tty_struct *tty);
 void (*break_ctl)(struct tty_struct *tty, int state);
 void (*flush_buffer)(struct tty_struct *tty);
 void (*set_ldisc)(struct tty_struct *tty);
 void (*wait_until_sent)(struct tty_struct *tty, int timeout);
 void (*send_xchar)(struct tty_struct *tty, char ch);
 int (*read_proc)(char *page, char **start, off_t off,
     int count, int *eof, void *data);
 int (*write_proc)(struct file *file, const char *buffer,
     unsigned long count, void *data);
 int (*tiocmget)(struct tty_struct *tty, struct file *file);
 int (*tiocmset)(struct tty_struct *tty, struct file *file,
   unsigned int set, unsigned int clear);
};

struct tty_driver {
 int magic;
 struct cdev cdev;
 struct module *owner;
 const char *driver_name;
 const char *name;
 int name_base;
 int major;
 int minor_start;
 int minor_num;
 int num;
 short type;
 short subtype;
 struct termios init_termios;
 int flags;
 int refcount;
 struct proc_dir_entry *proc_entry;





 int (*open)(struct tty_struct * tty, struct file * filp);
 void (*close)(struct tty_struct * tty, struct file * filp);
 int (*write)(struct tty_struct * tty,
        const unsigned char *buf, int count);
 void (*put_char)(struct tty_struct *tty, unsigned char ch);
 void (*flush_chars)(struct tty_struct *tty);
 int (*write_room)(struct tty_struct *tty);
 int (*chars_in_buffer)(struct tty_struct *tty);
 int (*ioctl)(struct tty_struct *tty, struct file * file,
      unsigned int cmd, unsigned long arg);
 void (*set_termios)(struct tty_struct *tty, struct termios * old);
 void (*throttle)(struct tty_struct * tty);
 void (*unthrottle)(struct tty_struct * tty);
 void (*stop)(struct tty_struct *tty);
 void (*start)(struct tty_struct *tty);
 void (*hangup)(struct tty_struct *tty);
 void (*break_ctl)(struct tty_struct *tty, int state);
 void (*flush_buffer)(struct tty_struct *tty);
 void (*set_ldisc)(struct tty_struct *tty);
 void (*wait_until_sent)(struct tty_struct *tty, int timeout);
 void (*send_xchar)(struct tty_struct *tty, char ch);
 int (*read_proc)(char *page, char **start, off_t off,
     int count, int *eof, void *data);
 int (*write_proc)(struct file *file, const char *buffer,
     unsigned long count, void *data);
 int (*tiocmget)(struct tty_struct *tty, struct file *file);
 int (*tiocmset)(struct tty_struct *tty, struct file *file,
   unsigned int set, unsigned int clear);
};



struct tty_driver *alloc_tty_driver(int lines);

void put_tty_driver(struct tty_driver *driver);

void tty_set_operations(struct tty_driver *driver,
   const struct tty_operations *op);







struct tty_ldisc {
 int magic;
 char *name;
 int num;
 int flags;




 int (*open)(struct tty_struct *);
 void (*close)(struct tty_struct *);
 void (*flush_buffer)(struct tty_struct *tty);
 ssize_t (*chars_in_buffer)(struct tty_struct *tty);
 ssize_t (*read)(struct tty_struct * tty, struct file * file,
   unsigned char * buf, size_t nr);
 ssize_t (*write)(struct tty_struct * tty, struct file * file,
    const unsigned char * buf, size_t nr);
 int (*ioctl)(struct tty_struct * tty, struct file * file,
    unsigned int cmd, unsigned long arg);
 void (*set_termios)(struct tty_struct *tty, struct termios * old);
 unsigned int (*poll)(struct tty_struct *, struct file *,
        struct poll_table_struct *);
 int (*hangup)(struct tty_struct *tty);




 void (*receive_buf)(struct tty_struct *, const unsigned char *cp,
          char *fp, int count);
 void (*write_wakeup)(struct tty_struct *);

 struct module *owner;

 int refcount;
};
struct tty_struct {
 int magic;
 struct tty_driver *driver;
 int index;
 struct termios *termios, *termios_locked;
 char name[64];

 unsigned long flags;
 int count;
 unsigned char stopped:1, hw_stopped:1, flow_stopped:1, packet:1;
 unsigned int receive_room;

 wait_queue_head_t write_wait;
 wait_queue_head_t read_wait;
 void *disc_data;
 void *driver_data;

 unsigned char closing:1;
};
extern struct termios tty_std_termios;


extern int tty_check_change(struct tty_struct * tty);
extern int tty_register_ldisc(int disc, struct tty_ldisc *new_ldisc);
extern int tty_unregister_ldisc(int disc);
extern int tty_register_driver(struct tty_driver *driver);
extern int tty_unregister_driver(struct tty_driver *driver);

extern void tty_wait_until_sent(struct tty_struct * tty, long timeout);


extern void tty_hangup(struct tty_struct * tty);
extern int tty_hung_up_p(struct file * filp);
extern void do_SAK(struct tty_struct *tty);
extern void tty_flip_buffer_push(struct tty_struct *tty);
extern int tty_get_baud_rate(struct tty_struct *tty);

extern void tty_wakeup(struct tty_struct *tty);
extern void tty_ldisc_flush(struct tty_struct *tty);






struct ddv_tty_driver {
  struct tty_driver driver;
  unsigned short allocated;
  unsigned short registered;
};

struct ddv_tty_driver global_tty_driver;

struct tty_driver *alloc_tty_driver(int lines)
{
  if (!global_tty_driver.allocated) {
    global_tty_driver.driver.magic = 0x5402;
    global_tty_driver.driver.num = lines;
  } else {
    return ((void *)0);
  }
}

void tty_set_operations(struct tty_driver *driver,
   const struct tty_operations *op)
{
  driver->open = op->open;
  driver->close = op->close;
  driver->write = op->write;
  driver->put_char = op->put_char;
  driver->flush_chars = op->flush_chars;
  driver->write_room = op->write_room;
  driver->chars_in_buffer = op->chars_in_buffer;
  driver->ioctl = op->ioctl;
  driver->set_termios = op->set_termios;
  driver->throttle = op->throttle;
  driver->unthrottle = op->unthrottle;
  driver->stop = op->stop;
  driver->start = op->start;
  driver->hangup = op->hangup;
  driver->break_ctl = op->break_ctl;
  driver->flush_buffer = op->flush_buffer;
  driver->set_ldisc = op->set_ldisc;
  driver->wait_until_sent = op->wait_until_sent;
  driver->send_xchar = op->send_xchar;
  driver->read_proc = op->read_proc;
  driver->write_proc = op->write_proc;
  driver->tiocmget = op->tiocmget;
  driver->tiocmset = op->tiocmset;
}
inline int alloc_chrdev_region(dev_t *dev, unsigned baseminor, unsigned count, const char *name)
{
    int major;
    int return_value = __VERIFIER_nondet_int();
    assume_abort_if_not((return_value == 0) || (return_value == -1));

    if (return_value == 0) {
        major = __VERIFIER_nondet_uint();
 *dev = (((major) << 20) | (baseminor));
    }

    return return_value;
}

inline int register_chrdev_region(dev_t from, unsigned count, const char *name)
{
    int return_value = __VERIFIER_nondet_int();
    assume_abort_if_not((return_value == 0) || (return_value == -1));

    return return_value;
}



inline int register_chrdev(
  unsigned int major, const char *name,
  struct file_operations *fops)
{
    struct cdev *cdev;
    int err;

    major = register_chrdev_region(0, 256, name);

    cdev = cdev_alloc();
    cdev->owner = fops->owner;
    cdev->ops = fops;

    err = cdev_add(cdev, (((major) << 20) | (0)), 256);

    if (err) {
 kfree(cdev);
 return err;
    }

    return major;
}

inline int unregister_chrdev(unsigned int major, const char *name)
{
    return 0;
}

inline struct cdev *cdev_alloc(void)
{
  if (fixed_cdev_used < 1) {
    return &fixed_cdev[fixed_cdev_used++];
  }
}

inline void cdev_init(struct cdev *cdev, struct file_operations *fops)
{
    cdev->ops = fops;
}

inline int cdev_add(struct cdev *p, dev_t dev, unsigned count)
{
    p->dev = dev;
    p->count = count;

    int return_value = __VERIFIER_nondet_int();
    assume_abort_if_not((return_value == 0) || (return_value == -1));

    if (return_value == 0) {
 if (number_cdev_registered < 1) {

     cdev_registered[number_cdev_registered].cdevp = p;
     cdev_registered[number_cdev_registered].inode.i_rdev = dev;
     cdev_registered[number_cdev_registered].inode.i_cdev = p;
     cdev_registered[number_cdev_registered].open = 0;

     number_cdev_registered++;
 } else {
     return -1;
 }
    }

    return return_value;
}

inline void cdev_del(struct cdev *p)
{
    int i;

    for (i = 0; i < number_cdev_registered; i++) {
 if (cdev_registered[i].cdevp == p) {
     cdev_registered[i].cdevp = 0;

     return;
 }
    }
}



inline void mutex_init(struct mutex *lock)
{
 __VERIFIER_HIDE:



    lock->locked = 0;
    lock->init = 1;
}

inline void mutex_lock(struct mutex *lock)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();
    assert_context_process();




    lock->locked = 1;
    __VERIFIER_atomic_end();
}

inline void mutex_unlock(struct mutex *lock)
{
 __VERIFIER_HIDE:
    assert_context_process();



    lock->locked = 0;
}




int ddv_ioport_request_start;
int ddv_ioport_request_len;

inline struct resource *request_region(unsigned long start, unsigned long len, const char *name)
{
  unsigned int i;
    struct resource *resource = (struct resource*)malloc(sizeof(struct resource));




    ddv_ioport_request_start = start;
    ddv_ioport_request_len = len;

    return resource;
}

inline void release_region(unsigned long start, unsigned long len)
{
  unsigned int i = 0;





  ddv_ioport_request_start = 0;
  ddv_ioport_request_len = 0;
}

inline unsigned char inb(unsigned int port)
{
 __VERIFIER_HIDE:
    __VERIFIER_assert(port >= ddv_ioport_request_start && port < ddv_ioport_request_start + ddv_ioport_request_len, "I/O port is requested");

    return __VERIFIER_nondet_uchar();
}

inline void outb(unsigned char byte, unsigned int port)
{
}

inline unsigned short inw(unsigned int port)
{
    return __VERIFIER_nondet_ushort();
}

inline void outw(unsigned short word, unsigned int port)
{
}

inline unsigned inl(unsigned int port)
{
    return __VERIFIER_nondet_unsigned();
}

inline void outl(unsigned doubleword, unsigned int port)
{
}

inline unsigned char inb_p(unsigned int port)
{
    return __VERIFIER_nondet_uchar();
}

inline void outb_p(unsigned char byte, unsigned int port)
{
}

inline unsigned short inw_p(unsigned int port)
{
    return __VERIFIER_nondet_ushort();
}

inline void outw_p(unsigned short word, unsigned int port)
{
}

inline unsigned inl_p(unsigned int port)
{
    return __VERIFIER_nondet_unsigned();
}

inline void outl_p(unsigned doubleword, unsigned int port)
{
}







void schedule()
{
    assert_context_process();
}

long schedule_timeout(long timeout)
{
    assert_context_process();

    return __VERIFIER_nondet_long();
}





inline void sema_init(struct semaphore *sem, int val)
{
    sem->init = 1;
    sem->locked = 0;
}

inline void init_MUTEX(struct semaphore * sem)
{
    sem->init = 1;
    sem->locked = 0;
}

inline void init_MUTEX_LOCKED(struct semaphore * sem)
{
    sem->init = 1;
    sem->locked = 1;
}

inline void down(struct semaphore * sem)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();
    assert_context_process();




    sem->locked = 1;
    __VERIFIER_atomic_end();
}

inline int down_interruptible(struct semaphore * sem)
{
  if (__VERIFIER_nondet_int()) {
     __VERIFIER_HIDE:
     __VERIFIER_atomic_begin();
     assert_context_process();




     sem->locked = 1;
     __VERIFIER_atomic_end();

     return 0;
  } else {
      return -1;
  }
}

inline int down_trylock(struct semaphore * sem)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();
    assert_context_process();





    if (sem->locked == 0) {
      sem->locked = 1;
      return 0;
    } else {
      return 1;
    }

    __VERIFIER_atomic_end();

    return 0;
}

inline void up(struct semaphore * sem)
{
 __VERIFIER_HIDE:
     assert_context_process();



    sem->locked = 0;
}



inline void tasklet_schedule(struct tasklet_struct *t)
{
    int i;
    int next_free = -1;





    for (i = 0; i < 1; i++) {
 if (tasklet_registered[i].tasklet == ((void *)0)) {
     next_free = i;
 }
 if ((tasklet_registered[i].tasklet == t) &&
     (tasklet_registered[i].is_running == 0)) {
     return;
 }
    }

    if (next_free == -1) {

    }

    tasklet_registered[next_free].tasklet = t;
    tasklet_registered[next_free].is_running = 0;
}

inline void tasklet_init(
  struct tasklet_struct *t,
  void (*func)(unsigned long), unsigned long data)
{
    t->count = 0;
    t->init = 0;
    t->func = func;
    t->data = data;
}



inline void spin_lock_init(spinlock_t * lock)
{
    lock->init = 1;
    lock->locked = 0;
}

inline void spin_lock(spinlock_t * lock)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();




    lock->locked = 1;
    __VERIFIER_atomic_end();
}

inline void spin_lock_irqsave(spinlock_t *lock, unsigned long flags)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();




    lock->locked = 1;
    __VERIFIER_atomic_end();
}

inline void spin_lock_irq(spinlock_t *lock)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();




    lock->locked = 1;
    __VERIFIER_atomic_end();
}

inline void spin_lock_bh(spinlock_t *lock)
{
 __VERIFIER_HIDE:
    __VERIFIER_atomic_begin();




    lock->locked = 1;
    __VERIFIER_atomic_end();
}

inline void spin_unlock(spinlock_t *lock)
{
 __VERIFIER_HIDE:



    lock->locked = 0;
}

inline void spin_unlock_irqrestore(spinlock_t *lock, unsigned long flags)
{
 __VERIFIER_HIDE:



    lock->locked = 0;
}

inline void spin_unlock_irq(spinlock_t *lock)
{
 __VERIFIER_HIDE:



    lock->locked = 0;
}

inline void spin_unlock_bh(spinlock_t *lock)
{
 __VERIFIER_HIDE:



    lock->locked = 0;
}



inline void init_timer(struct timer_list * timer)
{
    if (number_timer_registered < 1) {
 timer->__ddv_active = 0;
 timer->__ddv_init = 1;
 timer_registered[number_timer_registered].timer = timer;

 number_timer_registered++;
    }
}

inline void add_timer(struct timer_list *timer)
{




    timer->__ddv_active = 1;
}

inline void add_timer_on(struct timer_list *timer, int cpu)
{

    add_timer(timer);
}

inline int del_timer(struct timer_list * timer)
{
    timer->__ddv_active = 0;
}

inline int mod_timer(struct timer_list *timer, unsigned long expires)
{




    timer->expires = expires;
    timer->__ddv_active = 1;
}


inline void init_waitqueue_head(wait_queue_head_t *q)
{
  q->init = 1;
}

inline void wake_up(wait_queue_head_t *q)
{




}

inline void wake_up_all(wait_queue_head_t *q)
{




}

inline void wake_up_interruptible(wait_queue_head_t *q)
{




}

inline void sleep_on(wait_queue_head_t *q)
{




}

inline void interruptible_sleep_on(wait_queue_head_t *q)
{




}




inline int schedule_work(struct work_struct *work)
{
    int i;






    for (i = 0; i < 10; i++) {
 if (shared_workqueue[i] == work) {
     return 0;
 }

 if (shared_workqueue[i] == ((void *)0)) {
     shared_workqueue[i] = work;

     return 1;
 }
    }


    return -1;
}

inline void call_shared_workqueue_functions()
{
    unsigned short i = __VERIFIER_nondet_ushort();
    assume_abort_if_not(i < 10);

    if (shared_workqueue[i] != ((void *)0)) {
 (*shared_workqueue[i]->func)(shared_workqueue[i]->data);
 shared_workqueue[i] = ((void *)0);
    }
}



int request_irq(unsigned int irq, irq_handler_t handler,
  unsigned long irqflags, const char * devname, void *dev_id)
{
    if (__VERIFIER_nondet_int()) {
 registered_irq[irq].handler = handler;
 registered_irq[irq].dev_id = dev_id;

 return 0;
    } else {
        return -1;
    }
}

void free_irq(unsigned int irq, void *dev_id)
{
    registered_irq[irq].handler = ((void *)0);
    registered_irq[irq].dev_id = ((void *)0);
}




inline unsigned long __get_free_pages(gfp_t gfp_mask, unsigned int order)
{
 __VERIFIER_HIDE:
    if (gfp_mask & (( gfp_t)0x10u)) {
 assert_context_process();
    }
}

inline unsigned long __get_free_page(gfp_t gfp_mask)
{
 __VERIFIER_HIDE:
    if (gfp_mask & (( gfp_t)0x10u)) {
 assert_context_process();
    }
}

inline unsigned long get_zeroed_page(gfp_t gfp_mask)
{
 __VERIFIER_HIDE:
    if (gfp_mask & (( gfp_t)0x10u)) {
 assert_context_process();
    }
}

inline static struct page *alloc_pages_node(
  int nid, gfp_t gfp_mask, unsigned int order)
{
 __VERIFIER_HIDE:
    if (gfp_mask & (( gfp_t)0x10u)) {
 assert_context_process();
    }
}

inline struct page * alloc_pages(gfp_t gfp_mask, unsigned int order)
{
 __VERIFIER_HIDE:
    if (gfp_mask & (( gfp_t)0x10u)) {
 assert_context_process();
    }
}

inline struct page * alloc_page(gfp_t gfp_mask)
{
 __VERIFIER_HIDE:
    if (gfp_mask & (( gfp_t)0x10u)) {
 assert_context_process();
    }
}





void * kmalloc(size_t size, gfp_t flags)
{
    if (flags & (( gfp_t)0x10u)) {
 assert_context_process();
    }

    return malloc(size);
}

void * kzalloc(size_t size, gfp_t flags)
{
    if (flags & (( gfp_t)0x10u)) {
 assert_context_process();
    }

    return malloc(size);
}





void *vmalloc(unsigned long size);
void vfree(void *addr);




void * vmalloc(unsigned long size)
{
  return malloc(size);
}
int printk(const char *fmt, ...)
{
  return 0;
}

extern unsigned short __VERIFIER_nondet_ushort();
extern unsigned long __VERIFIER_nondet_ulong();
extern short __VERIFIER_nondet_short();
extern int __VERIFIER_nondet_int();
extern char __VERIFIER_nondet_char();
extern size_t __VERIFIER_nondet_size_t();
extern unsigned int __VERIFIER_nondet_uint();
extern unsigned char __VERIFIER_nondet_uchar();
extern unsigned __VERIFIER_nondet_unsigned();
extern long __VERIFIER_nondet_long();
extern char* __VERIFIER_nondet_pchar();
extern loff_t __VERIFIER_nondet_loff_t();
extern sector_t __VERIFIER_nondet_sector_t();
loff_t no_llseek(struct file *file, loff_t offset, int origin) { loff_t l; return l; }





int nonseekable_open(struct inode * inode, struct file * filp) { int i; return i; }
void __module_get(struct module *module) { }
int test_and_set_bit(int nr, unsigned long *addr)
{
  unsigned int bit = 1 << (nr & 31);
  addr += (nr >> 5);
  unsigned long old = *addr;
  *addr = old | bit;
  return (old & bit) != 0;
}

void clear_bit(int nr, volatile unsigned long *addr)
{
  unsigned int bit = 1 << (nr & 31);
  addr += (nr >> 5);
  unsigned long old = *addr;
  *addr = old & ~bit;
}

int register_reboot_notifier(struct notifier_block *dummy) { int i; return i; }
int misc_deregister(struct miscdevice * misc) { int i; return i; }
int unregister_reboot_notifier(struct notifier_block * dummy) { int i; return i; }
void release_mem_region(unsigned long start, unsigned long len) { }
void kfree(const void* addr) { }

struct resource *request_mem_region(unsigned long start, unsigned long len, const char *name)
{
  return (struct resource*)malloc(sizeof(struct resource));
};

void __VERIFIER_atomic_begin() { }
void __VERIFIER_atomic_end() { }
