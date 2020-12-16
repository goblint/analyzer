// PARAM: --set exp.malloc.wrappers "['ldv_malloc']"

#include <stdlib.h>

typedef unsigned long __kernel_ulong_t;
typedef __kernel_ulong_t __kernel_size_t;
typedef __kernel_size_t size_t;
typedef unsigned long long u64;
typedef u64 phys_addr_t;
typedef phys_addr_t resource_size_t;


struct usb_hcd {
  resource_size_t rsrc_start ;
};

struct resource {
  resource_size_t start ;
};


void *ldv_malloc(size_t size)
{
  return malloc(size);
}

struct usb_hcd *usb_create_hcd() {
  return ldv_malloc(sizeof(struct usb_hcd));

  // normal malloc is fine
  // return malloc(sizeof(struct usb_hcd));
}

static int isp1362_probe()
{
  struct usb_hcd *hcd ;
  struct resource *data ;

  hcd = usb_create_hcd();
  hcd->rsrc_start = data->start;
  return 0;
}

int main(void)
{
  isp1362_probe();
  return 0;
}