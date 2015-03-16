// SKIP PARAM: --set ana.activated[+] "'shape'"  --set nonstatic true
#include<linuxlight.h>

LIST_HEAD (usb_bus_list);
DEFINE_MUTEX(usb_bus_list_lock);

struct usb_bus {
  struct list_head bus_list;
  int busnum;
};

int usb_register_bus(struct usb_bus *bus) {
  mutex_lock(&usb_bus_list_lock);
  list_add (&bus->bus_list, &usb_bus_list);
  mutex_unlock(&usb_bus_list_lock);
  return 0;
}

int usb_deregister_bus(struct usb_bus *bus) {
  mutex_lock(&usb_bus_list_lock);
  list_del (&bus->bus_list);
  mutex_unlock(&usb_bus_list_lock);
  bus->busnum = 42; // NORACE
  return 0;
}
