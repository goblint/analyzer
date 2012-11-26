// PARAM: --set kernel true --set mainfun[+] "'test_init'"
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/miscdevice.h>
#include <linux/mutex.h>

static int data;
static DEFINE_MUTEX(mutex1);
static DEFINE_MUTEX(mutex2);

static ssize_t my_read(struct file *file, char __user *buf, size_t count, loff_t *ppos) {
  mutex_lock(&mutex1);
  data++; //NORACE!
  mutex_unlock(&mutex1);
  return 0;
}

static ssize_t my_write(struct file *fp, const char __user *buf, size_t len, loff_t *off) {
  mutex_lock(&mutex1);
  data++; //NORACE!
  mutex_unlock(&mutex1);
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
    printk(KERN_ERR "Unable to register testing device\n");
  return ret;
}


static void __exit test_exit(void) {
  misc_deregister(&test_dev);
}

module_init(test_init);
module_exit(test_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Vesal Vojdani <vojdanig@in.tum.de>");
