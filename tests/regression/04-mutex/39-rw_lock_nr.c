// PARAM: --kernel --mainfun test_init
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/miscdevice.h>
#include <linux/spinlock.h>

static int data;
static DEFINE_RWLOCK(rwlock);

static ssize_t my_read(struct file *file, char __user *buf, size_t count, loff_t *ppos) {
  int ret;
  read_lock(&rwlock);
  ret = data == 1; //NORACE!
  read_unlock(&rwlock);
  return ret;
}

static ssize_t my_write(struct file *fp, const char __user *buf, size_t len, loff_t *off) {
  write_lock(&rwlock);
  data = data==1 ? 0 : 1; //NORACE!
  write_unlock(&rwlock);
  return 0;
}

static const struct file_operations test_fops = {
  .owner = THIS_MODULE,
  .read	 = my_read,
  .write = my_write,
};

static struct miscdevice test_dev = {
  MISC_DYNAMIC_MINOR,
  "testing",
  &test_fops
};

static int __init test_init(void) {
  int ret;
  ret = misc_register(&test_dev);
  if (ret)
    printk(KERN_ERR,"Unable to register testing device\n");
  return ret;
}


static void __exit test_exit(void) {
  misc_deregister(&test_dev);
}

module_init(test_init);
module_exit(test_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Vesal Vojdani <vojdanig@in.tum.de>");
