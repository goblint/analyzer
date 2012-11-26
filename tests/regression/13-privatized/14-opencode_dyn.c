// PARAM: --set ana.activated[0][+] "'shape'"  --set nonstatic true --sets exp.unique[+] usb_drv
#include<linuxlight.h>

struct usb_drv { 
  struct list_head buses;
  struct mutex     buses_lock;
};

struct usb_bus {
  struct list_head bus_list;
  int busnum;
};

int usb_register_bus(struct usb_bus *bus, struct usb_drv *drv) {
  mutex_lock(&drv->buses_lock);
  list_add (&bus->bus_list, &drv->buses);
  mutex_unlock(&drv->buses_lock);
  return 0;
}

int access(struct usb_drv *drv) {
  struct usb_bus *bus;
  mutex_lock(&drv->buses_lock);
  list_for_each_entry(bus, &drv->buses, bus_list) {
    bus->busnum = 42; // NORACE
  }
  mutex_unlock(&drv->buses_lock);
  return 0;
}
