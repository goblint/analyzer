// PARAM: --enable kernel
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/spinlock.h>
#include <linux/ipmi.h>
#include <linux/init.h>
#include <linux/device.h>
#include <linux/compat.h>

struct ipmi_file_private
{
    ipmi_user_t          user;
    spinlock_t           recv_msg_lock;
    struct list_head     recv_msgs;
    struct file          *file;
    struct fasync_struct *fasync_queue;
    wait_queue_head_t    wait;
    struct mutex	     recv_mutex;
    int                  default_retries;
    unsigned int         default_retry_time_ms;
};

static DEFINE_MUTEX(ipmi_mutex);

static struct ipmi_user_hndl ipmi_hndlrs = {
};

static int ipmi_open(struct inode *inode, struct file *file)
{
    struct ipmi_file_private *priv;
    priv = kmalloc(sizeof(*priv), GFP_KERNEL);
    mutex_lock(&ipmi_mutex);
    priv->file = file; // should reach fixpoint from priv side effect from here

    ipmi_create_user(0, &ipmi_hndlrs, priv, &(priv->user));
    file->private_data = priv;

    spin_lock_init(&(priv->recv_msg_lock));
    INIT_LIST_HEAD(&(priv->recv_msgs));
    init_waitqueue_head(&priv->wait);
    priv->fasync_queue = NULL;
    mutex_init(&priv->recv_mutex);

    /* Use the low-level defaults. */
    priv->default_retries = -1;
    priv->default_retry_time_ms = 0;

    mutex_unlock(&ipmi_mutex);
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
