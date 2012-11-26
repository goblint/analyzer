// SKIP PARAM: --set ana.activated[0][+] "'shape'"  --set kernel true
#include<linux/module.h>
#include<linux/miscdevice.h>
#include<linux/init.h>
#include<linux/slab.h>
#include<linux/fs.h>
#include<linux/list.h>
#include<linux/mutex.h>

struct node {
  int datum;
  struct list_head list;
}; 

struct list_head A, B;

DEFINE_MUTEX(mutex_A);
DEFINE_MUTEX(mutex_B);

static ssize_t *generate(struct file *fp, const char __user *buf, size_t len, loff_t *off) {
  int i;
  struct node *p; 
  for (i=1; i<100; i++) {
    p = kmalloc(sizeof(struct node), GFP_KERNEL);
    INIT_LIST_HEAD(&p->list);
    p->datum = i; // NORACE
    mutex_lock(&mutex_A);
    list_add(&p->list, &A); 
    mutex_unlock(&mutex_A);
  }
  return 0;
}

static ssize_t *process(struct file *file, char __user *buf, size_t count, loff_t *ppos) {
  struct list_head *p;
  while (1) {
    mutex_lock(&mutex_A);
    if (! list_empty(&A)) {
      p = A.next;
      mutex_lock(&mutex_B);
      list_move(p,&B);
      mutex_unlock(&mutex_B);
      mutex_unlock(&mutex_A);
    }
    else
      mutex_unlock(&mutex_A);
  }
  return 0;
}

static int *dispose(struct inode *inode, struct file *file) {
  struct list_head *p;
  struct node *q;
  while (1) {
    mutex_lock(&mutex_B);
    if (! list_empty(&B)) {
      p = B.next;
      list_del(p);
      mutex_unlock(&mutex_B);
      q = list_entry(p, struct node, list);
      q->datum = 0; // NORACE
    }
    else
      mutex_unlock(&mutex_B);
  }
  return NULL;
}

loff_t traverse(struct file *file, loff_t offset, int origin) {
  int i;
  struct node *p;
  int sum = 0;

  for (i=0; i<10; i++) {
    mutex_lock(&mutex_A);
    list_for_each_entry(p, &A, list)
      sum += p->datum; // NORACE
    mutex_unlock(&mutex_A);

    mutex_lock(&mutex_B);
    list_for_each_entry(p, &B, list)
      sum += p->datum; // NORACE
    mutex_unlock(&mutex_B);
  }
  return 0;
}

static const struct file_operations test_fops = {
  .owner = THIS_MODULE,
  .write = generate,
  .read	 = process,
  .release = dispose,
  .llseek = traverse
};

static struct miscdevice test_dev = {
  MISC_DYNAMIC_MINOR,
  "testing",
  &test_fops
};

static int __init test_init(void) {
  int ret;
  INIT_LIST_HEAD(&A);
  INIT_LIST_HEAD(&B);
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
