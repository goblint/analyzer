// SKIP PARAM: --set ana.activated[+] "'shape'"  --set nonstatic true --sets exp.unique[+] usb_drv --sets exp.unique[+] usb_bus --set exp.region-offsets true
#include<linuxlight.h>

struct usb_drv { 
  struct list_head buses;
  struct mutex     buses_lock;
};

struct usb_bus {
  struct list_head bus_list;
  struct mutex     data_lock;
  int busnum;
};

int usb_register_bus(struct usb_bus *bus, struct usb_drv *drv) {
  mutex_lock(&drv->buses_lock);
  list_add (&bus->bus_list, &drv->buses);
  mutex_unlock(&drv->buses_lock);
  return 0;
}

int access_all(struct usb_drv *drv) {
  struct usb_bus *bus;
  mutex_lock(&drv->buses_lock);
  list_for_each_entry(bus, &drv->buses, bus_list) {
    mutex_lock(&bus->data_lock);
    bus->busnum = 42; // NORACE
    mutex_unlock(&bus->data_lock);
  }
  mutex_unlock(&drv->buses_lock);
  return 0;
}

int access(struct usb_bus *bus) {
  mutex_lock(&bus->data_lock);
  bus->busnum = 42; // NORACE
  mutex_unlock(&bus->data_lock);
  return 0;
}
