typedef unsigned int size_t;
typedef long long __quad_t;
typedef long __off_t;
typedef __quad_t __off64_t;
typedef long __time_t;
typedef __time_t time_t;
typedef int __ssize_t;
typedef __ssize_t ssize_t;
struct __anonstruct___sigset_t_7 {
   unsigned long __val[(int )(1024U / (8U * sizeof(unsigned long )))] ;
};
typedef struct __anonstruct___sigset_t_7 __sigset_t;
typedef __sigset_t sigset_t;
struct __sched_param {
   int __sched_priority ;
};
struct __pthread_attr_s {
   int __detachstate ;
   int __schedpolicy ;
   struct __sched_param __schedparam ;
   int __inheritsched ;
   int __scope ;
   size_t __guardsize ;
   int __stackaddr_set ;
   void *__stackaddr ;
   size_t __stacksize ;
};
typedef struct __pthread_attr_s pthread_attr_t;
typedef unsigned long pthread_t;
struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef void _IO_lock_t;
struct _IO_marker {
   struct _IO_marker *_next ;
   struct _IO_FILE *_sbuf ;
   int _pos ;
};
struct _IO_FILE {
   int _flags ;
   char *_IO_read_ptr ;
   char *_IO_read_end ;
   char *_IO_read_base ;
   char *_IO_write_base ;
   char *_IO_write_ptr ;
   char *_IO_write_end ;
   char *_IO_buf_base ;
   char *_IO_buf_end ;
   char *_IO_save_base ;
   char *_IO_backup_base ;
   char *_IO_save_end ;
   struct _IO_marker *_markers ;
   struct _IO_FILE *_chain ;
   int _fileno ;
   int _flags2 ;
   __off_t _old_offset ;
   unsigned short _cur_column ;
   signed char _vtable_offset ;
   char _shortbuf[1] ;
   _IO_lock_t *_lock ;
   __off64_t _offset ;
   void *__pad1 ;
   void *__pad2 ;
   int _mode ;
   char _unused2[(int )(15U * sizeof(int ) - 2U * sizeof(void *))] ;
};
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned short sa_family_t;
struct sockaddr {
   sa_family_t sa_family ;
   char sa_data[14] ;
};
typedef uint16_t in_port_t;
typedef uint32_t in_addr_t;
struct in_addr {
   in_addr_t s_addr ;
};
struct sockaddr_in {
   sa_family_t sin_family ;
   in_port_t sin_port ;
   struct in_addr sin_addr ;
   unsigned char sin_zero[(int )(((sizeof(struct sockaddr ) - sizeof(unsigned short )) -
                                  sizeof(in_port_t )) - sizeof(struct in_addr ))] ;
};
typedef long __suseconds_t;
struct timeval {
   __time_t tv_sec ;
   __suseconds_t tv_usec ;
};
struct timezone {
   int tz_minuteswest ;
   int tz_dsttime ;
};
typedef struct timezone * __restrict  __timezone_ptr_t;
typedef unsigned long long __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long __ino_t;
typedef unsigned int __mode_t;
typedef unsigned int __nlink_t;
typedef long __blksize_t;
typedef long __blkcnt_t;
struct timespec {};
struct stat {
   __dev_t st_dev ;
   unsigned short __pad1 ;
   __ino_t st_ino ;
   __mode_t st_mode ;
   __nlink_t st_nlink ;
   __uid_t st_uid ;
   __gid_t st_gid ;
   __dev_t st_rdev ;
   unsigned short __pad2 ;
   __off_t st_size ;
   __blksize_t st_blksize ;
   __blkcnt_t st_blocks ;
   struct timespec st_atim ;
   struct timespec st_mtim ;
   struct timespec st_ctim ;
   unsigned long __unused4 ;
   unsigned long __unused5 ;
};

/* int gettimeofday(struct timeval * __restrict  __tv , __timezone_ptr_t __tz ) { */
/*   __tv->tv_sec = 0; */
/*   __tv->tv_usec = 0; */
/*   // ignores the __tz pointer according to the manpage */
/*   return 0; */
/* } */

void* malloc(size_t);

int unlink(const char *pathname) {
  return 0;
}

size_t fwrite(const  void  *ptr,  size_t  size,  size_t  nmemb,  FILE *stream){
  int *i = (int*)ptr;
  *i;
  return 0; }

size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  *((int*)ptr) = 0;
  return 0;
}


FILE *fopen(const char *filename, const char *mode) {
  return 0;
}
FILE *fopen64(const char *filename, const char *mode) {
  return 0;
}

int fclose(FILE *stream) {return 0;}
int close(int fd) { return 0; }
int open(const char *pathname, int flags, ...) {
  char i = *pathname;
  return 0;
}

uint16_t htons(uint16_t hostshort) {
  return 0;
}

ssize_t write(int fd, const void *buf, size_t count) {
  char *t = (char*)buf;
  char i = t[0];
  return 0;
}

ssize_t read(int fd, const void *buf, size_t count) {
  ((int*)buf)[0] = 0;
  return 0;
}

int dup(int fd) { return 0; }

char *strdup(const char *s) {
  char *ss = (char*) malloc(3);
  *ss = *s;
  return ss;
}

int printf(const char *format, ...) {
  char i = *format;
  return 0;
}
int fprintf(FILE *stream, const char *format, ...) {
  FILE s = *stream;
  char i = *format;
  return 0;
}

time_t time(time_t *t) {
  static time_t a;
  t = &a;
  return a;
}


int pthread_setcanceltype(int type, int *oldtype) {
  *oldtype = 0;
  return 0;
}

pthread_t pthread_self(void) {return 0;}

ssize_t pread(int fd, void *buf, size_t count, __off_t offset)
{
  *((char*)buf) = 0;
  return 0;
}

ssize_t pwrite(int fd, const void *buf, size_t count, __off_t offset)
{
  int i = *((char*)buf);
  return 0;
}

unsigned int alarm(unsigned int seconds) {return 0;}

int sigwait(const sigset_t *set, int *sig) {
  static sigset_t s;
  s = *set;
  *sig = 0;
  return 0;
}

char *inet_ntoa(struct in_addr in) {
  static char b[100];
  return b;
}

struct hostent {};

struct hostent *gethostbyname(const char *name) {
  static struct hostent r;
  char s = *name;
  return &r;
}

/* To avoid spurious sharing (errno is thread-local), we just return
   NULL */
int *__errno_location(void)  {
  return (int *)0;
}

int *__h_errno_location(void) {
  return (int *)0;
}

ssize_t recv(int s, void *buf, size_t len, int flags) {
  ((char*)buf)[1] = 0;
  return 0;
}

ssize_t send(int s, const void *buf, size_t len, int flags) {
  int a = ((char*)buf)[1];
  return 0;
}

const char *hstrerror(int err) {
  return "foo";
}

typedef unsigned int socklen_t;

/* int setsockopt(int __fd , int __level , int __optname , void const   *__optval , */
/* 	       socklen_t __optlen ) { */
/*   int x = *(int *)__optval; */
/*   return 0; */
/* } */

int connect(int  sockfd, const struct sockaddr *serv_addr, socklen_t addrlen)
{
  struct sockaddr s = *serv_addr;
  return 0;
}

void bzero(void *s, size_t n) {
  ((char*)s)[0] = 0;
}


char *strtok(char *s, const char *delim) {
  static char *local;
  local = s;
  s[1] = *delim;
  return s;
}

char *strstr(const char *haystack, const char *needle) {
  char s = *needle;
  return (char*)haystack;
}

void free(void * ptr) {
  *((char*)ptr) = 0;
  return;
}

int socket(int domain, int type, int protocol) { return 0; }

void *memset(void *s, int c, size_t n) {
  ((char*)s)[0] = 0;
  return s;
}

int putchar(int c) {return 0;}

int fflush(FILE *stream) {
  return 0;
}

size_t strlen(const char *s) {
  char i = s[1];
  return 0;
}

int pthread_join(pthread_t th, void **thread_return) {
  ((char*)thread_return)[0] = 0;
  return 0;
}

int pthread_sigmask(int how, const sigset_t *newmask,  sigset_t  *oldmask) {
  sigset_t s;
  *oldmask = s;
  s = *newmask;
  return 0;
}

char *strerror(int errnum) {
  return "foo";
}

__off_t lseek(int fildes, __off_t offset, int whence) {
  return offset;
}

typedef __builtin_va_list va_list;

int vfprintf(FILE *stream, char const *fmt, va_list ap) {
  char s = fmt[0];
  return 0;
}

int pthread_cancel(pthread_t thread) {
  return 0;
}

int getopt(int argc, char * const argv[], const char *optstring) {
  char s = *optstring;
  s = argv[0][0];
  return s;
}

int sprintf(char *str, const char *format, ...) {
  char s = *format;
  str[0] = s;
  return 0;
}

int snprintf(char *str, size_t size, const char *format, ...) {
  char s = *format;
  str[0] = s;
  return 0;
}

int atoi(const char *nptr) {
  return *nptr;
}

in_addr_t inet_addr(const char *cp) {
  in_addr_t ss;
  char s;
  s = *cp;
  return ss;
}
       
int sigaddset(sigset_t *set, int signum) {
  sigset_t s;
  set[1] = s;
  return 0;
}

int sigemptyset(sigset_t *set) {
  sigset_t s;
  set[0] = s;
  return 0;
}

char *__builtin_strchr(const char *s, int c) {
  c = *s;
  return (char*)s;
}

char * __builtin_strpbrk(const char *s1, const char *s2) {
  int s = *s1;
  s = *s2;
  return (char*)s1;
}
char * __builtin_strcat(char *dest, const char *src) {
  *dest = *src;
  return dest;
}

char * __builtin_strncat(char *dest, const char *src, size_t n) {
  *dest = *src;
  return dest;
}

long double __builtin_fabsl(long double x) { return 0; }
double __builtin_fabs(double x) { return 0; }

int isdigit(int c) {
  return 0;
}

int ftw(const char *dir, int (*fn)(const char *file, const struct stat
				   *sb, int flag), int nopenfd) {
  fn("", 0, 0);
}
