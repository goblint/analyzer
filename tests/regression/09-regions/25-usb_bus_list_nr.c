// SKIP PARAM: --set ana.activated[+] "'region'"  --set kernel true --set nonstatic true --set exp.region-offsets true
#include<linux/module.h>
#include<linux/list.h>
#include<linux/mutex.h>

struct dev {
  int id;
  struct mutex mtx;
  int   data;
};

struct usb_bus {
  struct list_head list;
  struct dev       *root_hub;
};

LIST_HEAD   (bus_list);
DEFINE_MUTEX(bus_list_lock);

static
struct dev * find_dev(int id){
  struct list_head  *buslist;
  struct usb_bus    *bus;
  struct usb_device *dev = NULL;

  mutex_lock(&bus_list_lock);

  for (buslist  = bus_list.next;
       buslist != &bus_list;
       buslist  = buslist->next) {

    bus = container_of(buslist, struct usb_bus, list);

    mutex_lock(&bus->root_hub->mtx);

    if (bus->root_hub->id == id)
      dev = bus->root_hub;

    mutex_unlock(&bus->root_hub->mtx);

    if (dev)
      goto exit;
  }
exit:
  mutex_unlock(&bus_list_lock);
  return dev;
}

static
void add_bus(struct dev * d){
  struct usb_bus * bus;
  bus = (struct usb_bus *)malloc(sizeof(struct usb_bus));
  bus->root_hub = d;
  INIT_LIST_HEAD(&bus->list);

  mutex_lock(&bus_list_lock);
  list_add (&bus->list, &bus_list);
  mutex_unlock(&bus_list_lock);
}

void do_work() {
  int id;
  struct dev * dev;

  dev = find_dev(id);

  if (dev){
    mutex_lock(&dev->mtx);
    dev->data ++;
    mutex_unlock(&dev->mtx);
  }
}

void init() {
  int i1, i2;
  struct dev *dev1, *dev2;

  dev1 = (struct dev*)malloc(sizeof(struct dev));
  dev1->id = 1;
  mutex_init(&dev1->mtx);

  dev2 = (struct dev*)malloc(sizeof(struct dev));
  dev2->id = 2;
  mutex_init(&dev2->mtx);

  while(i1 > 0) {
    add_bus(dev1);
    add_bus(dev2);
    i1--;
  }
}
