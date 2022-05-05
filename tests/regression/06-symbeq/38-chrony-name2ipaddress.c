// PARAM: --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'" --set ana.activated[+] "'mallocFresh'" --set ana.malloc.wrappers '["Malloc"]' --disable sem.unknown_function.spawn --disable sem.unknown_function.invalidate.globals --set pre.cppflags[+] -D_FORTIFY_SOURCE=2 --set pre.cppflags[+] -O3
#include <stddef.h>
#include <stdint.h>
// #include <sys/types.h>
// #include <sys/socket.h>
#include <netdb.h>
#include <pthread.h>
#include <string.h>

void *
Malloc(size_t size)
{
  void *r;

  r = malloc(size);
  // if (!r && size)
  //   LOG_FATAL("Could not allocate memory");

  return r;
}

typedef enum {
  DNS_Success,
  DNS_TryAgain,
  DNS_Failure
} DNS_Status;

typedef struct {
  union {
    uint32_t in4;
    uint8_t in6[16];
    uint32_t id;
  } addr;
  uint16_t family;
  uint16_t _pad;
} IPAddr;

#define DNS_MAX_ADDRESSES 16
#define IPADDR_UNSPEC 0
#define IPADDR_INET4 1
#define IPADDR_INET6 2
#define IPADDR_ID 3

#define FEAT_IPV6 1

static int address_family = IPADDR_UNSPEC;

int
UTI_StringToIP(const char *addr, IPAddr *ip)
{
  struct in_addr in4;
#ifdef FEAT_IPV6
  struct in6_addr in6;
#endif

  if (inet_pton(AF_INET, addr, &in4) > 0) {
    ip->family = IPADDR_INET4;
    ip->_pad = 0;
    ip->addr.in4 = ntohl(in4.s_addr);
    return 1;
  }

#ifdef FEAT_IPV6
  if (inet_pton(AF_INET6, addr, &in6) > 0) {
    ip->family = IPADDR_INET6;
    ip->_pad = 0;
    memcpy(ip->addr.in6, in6.s6_addr, sizeof (ip->addr.in6));
    return 1;
  }
#endif

  return 0;
}


#define MIN(x, y) ((x) < (y) ? (x) : (y))

DNS_Status
DNS_Name2IPAddress(const char *name, IPAddr *ip_addrs, int max_addrs)
{
  struct addrinfo hints, *res, *ai;
  int i, result;
  IPAddr ip;

  max_addrs = MIN(max_addrs, DNS_MAX_ADDRESSES);

  for (i = 0; i < max_addrs; i++)
    ip_addrs[i].family = IPADDR_UNSPEC;

// #if 0
  /* Avoid calling getaddrinfo() if the name is an IP address */
  if (UTI_StringToIP(name, &ip)) {
    if (address_family != IPADDR_UNSPEC && ip.family != address_family)
      return DNS_Failure;
    if (max_addrs >= 1)
      ip_addrs[0] = ip;
    return DNS_Success;
  }

  memset(&hints, 0, sizeof (hints));

  switch (address_family) {
    case IPADDR_INET4:
      hints.ai_family = AF_INET;
      break;
#ifdef FEAT_IPV6
    case IPADDR_INET6:
      hints.ai_family = AF_INET6;
      break;
#endif
    default:
      hints.ai_family = AF_UNSPEC;
  }
  hints.ai_socktype = SOCK_DGRAM;

  result = getaddrinfo(name, NULL, &hints, &res);

  if (result) {
#ifdef FORCE_DNSRETRY
    return DNS_TryAgain;
#else
    return result == EAI_AGAIN ? DNS_TryAgain : DNS_Failure;
#endif
  }

  for (ai = res, i = 0; i < max_addrs && ai != NULL; ai = ai->ai_next) {
    switch (ai->ai_family) {
      case AF_INET:
        if (address_family != IPADDR_UNSPEC && address_family != IPADDR_INET4)
          continue;
        ip_addrs[i].family = IPADDR_INET4;
        ip_addrs[i].addr.in4 = ntohl(((struct sockaddr_in *)ai->ai_addr)->sin_addr.s_addr);
        i++;
        break;
#ifdef FEAT_IPV6
      case AF_INET6:
        if (address_family != IPADDR_UNSPEC && address_family != IPADDR_INET6)
          continue;
        /* Don't return an address that would lose a scope ID */
        if (((struct sockaddr_in6 *)ai->ai_addr)->sin6_scope_id != 0)
          continue;
        ip_addrs[i].family = IPADDR_INET6;
        memcpy(&ip_addrs[i].addr.in6, &((struct sockaddr_in6 *)ai->ai_addr)->sin6_addr.s6_addr,
               sizeof (ip_addrs->addr.in6));
        i++;
        break;
#endif
    }
  }

  freeaddrinfo(res);
// #endif

  return !max_addrs || ip_addrs[0].family != IPADDR_UNSPEC ? DNS_Success : DNS_Failure;
}

struct DNS_Async_Instance {
  const char *name;
  DNS_Status status;
  IPAddr addresses[DNS_MAX_ADDRESSES];
  // DNS_NameResolveHandler handler;
  // void *arg;
  pthread_mutex_t mutex;

  pthread_t thread;
  // int pipe[2];
};

static pthread_mutex_t privops_lock = PTHREAD_MUTEX_INITIALIZER;

/* ================================================== */

static void *
start_resolving(void *anything)
{
  struct DNS_Async_Instance *inst = (struct DNS_Async_Instance *)anything;

  pthread_mutex_lock(&inst->mutex);
  inst->status = DNS_Name2IPAddress(inst->name, inst->addresses, DNS_MAX_ADDRESSES);
  pthread_mutex_unlock(&inst->mutex);

  /* Notify the main thread that the result is ready */
  // if (write(inst->pipe[1], "", 1) < 0)
  //   ;

  return NULL;
}


#define MallocNew(T) ((T *) Malloc(sizeof(T)))

void
DNS_Name2IPAddressAsync(const char *name)
{
  struct DNS_Async_Instance *inst;

  inst = MallocNew(struct DNS_Async_Instance);
  inst->name = name;
  // inst->handler = handler;
  // inst->arg = anything;
  inst->status = DNS_Failure;
  pthread_mutex_init(&inst->mutex, NULL);

  // if (pipe(inst->pipe)) {
  //   LOG_FATAL("pipe() failed");
  // }

  // UTI_FdSetCloexec(inst->pipe[0]);
  // UTI_FdSetCloexec(inst->pipe[1]);

  if (pthread_create(&inst->thread, NULL, start_resolving, inst)) {
    // LOG_FATAL("pthread_create() failed");
  }

  // SCH_AddFileHandler(inst->pipe[0], SCH_FILE_INPUT, end_resolving, inst);
}

int main() {
  DNS_Name2IPAddressAsync("foo");
  DNS_Name2IPAddressAsync("bar");
  return 0;
}
