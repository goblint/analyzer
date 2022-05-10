// PARAM: --disable sem.unknown_function.invalidate.globals --disable sem.unknown_function.spawn
// extracted from ddverify pcwd

// header declarations

struct file_operations {
  void (*ioctl)();
};

struct miscdevice {
  struct file_operations *fops;
};

struct cdev {
  struct file_operations *ops;
};

// implementation stub

struct ddv_cdev {
  struct cdev *cdevp;
};

#define MAX_CDEV_SUPPORT 1

struct cdev fixed_cdev[MAX_CDEV_SUPPORT];
struct ddv_cdev cdev_registered[MAX_CDEV_SUPPORT];

int cdev_add(struct cdev *p)
{
  cdev_registered[0].cdevp = p;
  return 0;
}

int misc_register(struct miscdevice *misc) {
  fixed_cdev[0].ops = misc->fops;
  return cdev_add(&fixed_cdev[0]);
}

void call_cdev_functions()
{
  int cdev_no = 0;
  if (cdev_registered[cdev_no].cdevp->ops->ioctl) {
    (* cdev_registered[cdev_no].cdevp->ops->ioctl)();
  }
}

// concrete program

void pcwd_ioctl() {
  assert(1); // reachable
}

static const struct file_operations pcwd_fops = {
  .ioctl = pcwd_ioctl
};

static struct miscdevice pcwd_miscdev = {
  .fops = &pcwd_fops
};

int main() {
  misc_register(&pcwd_miscdev);

  void (*fp)(struct ddv_cdev*); // unknown function pointer
  fp(&cdev_registered); // invalidates argument!

  call_cdev_functions();
  return 0;
}
