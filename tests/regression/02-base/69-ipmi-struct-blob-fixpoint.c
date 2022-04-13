// PARAM: --enable kernel
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/init.h>
#include <linux/device.h>
#include <linux/compat.h>

struct ipmi_file_private
{
    struct file *file;
};

static DEFINE_MUTEX(ipmi_mutex);

static int ipmi_open(struct inode *inode, struct file *file)
{
    struct ipmi_file_private *priv;
    priv = kmalloc(sizeof(*priv), GFP_KERNEL);
    mutex_lock(&ipmi_mutex);
    priv->file = file; // should reach fixpoint from priv side effect from here
    return 0;
}

static const struct file_operations ipmi_fops = {
    .open = ipmi_open,
};

static int __init init_ipmi_devintf(void)
{
    register_chrdev(0, "ipmidev", &ipmi_fops);
    return 0;
}
module_init(init_ipmi_devintf);
