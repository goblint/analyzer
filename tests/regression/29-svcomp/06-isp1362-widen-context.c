// SKIP PARAM: --enable ana.context.widen
#include <stdlib.h>

typedef unsigned long __kernel_ulong_t;
typedef __kernel_ulong_t __kernel_size_t;
typedef __kernel_size_t size_t;
typedef unsigned long long u64;
typedef u64 phys_addr_t;
typedef phys_addr_t resource_size_t;


struct usb_hcd {
  resource_size_t rsrc_start ;
  unsigned long hcd_priv[0U] ;
};

struct isp1362_hcd {
  void *data_reg ;
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
}

struct isp1362_hcd *hcd_to_isp1362_hcd(struct usb_hcd *hcd )
{
  return ((struct isp1362_hcd *)(& hcd->hcd_priv));
}

static int isp1362_probe()
{
  struct usb_hcd *hcd ;
  struct isp1362_hcd *isp1362_hcd ;
  struct resource *data ;
  void *data_reg ;

  hcd = usb_create_hcd();
  hcd->rsrc_start = data->start;

  isp1362_hcd = hcd_to_isp1362_hcd(hcd);
  isp1362_hcd->data_reg = data_reg;
  return 0;
}

int main(void)
{
  isp1362_probe();
  return 0;
}