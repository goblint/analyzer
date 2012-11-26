// PARAM: --set kernel true --set dbg.debug true
#include <linux/module.h>
#include <linux/miscdevice.h>
#include <linux/device.h>

static struct class *misc_class;

static char *misc_devnode(struct device *dev, mode_t *mode)
{
  assert(false); // FAIL
  return NULL;
}

static int __init misc_init(void)
{
  misc_class = class_create(THIS_MODULE, "misc");
  register_chrdev(MISC_MAJOR,"misc", NULL);
        // Here misc_devnode is put into a shared object.
  misc_class->devnode = misc_devnode;
  return 0;
}
subsys_initcall(misc_init);


int misc_register(struct miscdevice * misc)
{
  dev_t dev;
  dev = MKDEV(MISC_MAJOR, misc->minor);
        // reference to misc_class, which contains the pointer, is passed along.
  misc->this_device = device_create(misc_class, misc->parent, dev,
            misc, "%s", misc->name);
  return 0;
}
EXPORT_SYMBOL(misc_register);

