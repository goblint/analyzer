// PARAM: --enable ana.int.interval --enable exp.partition-arrays.enabled --set ana.activated "['base', 'threadflag', 'mallocWrapper','assert']"
struct rtl8169_private {
   unsigned int features ;
};

struct net_device {
   unsigned long long features;
   struct rtl8169_private dings;
};


__inline static void *netdev_priv(struct net_device  const  *dev )
{
  {
   return ((void *)dev + 64U); // Some pointer arithmetic (but ok in the example)
  }
}


static int rtl_init_one()
{
  struct rtl8169_private *tp ;
  struct net_device *dev ;
  void *tmp ;

  dev = malloc(sizeof(struct net_device));
  dev->features = 17;
  tmp = netdev_priv((struct net_device  const  *)dev);
  tp = (struct rtl8169_private *)tmp;


  tp->features = 1;
  unsigned long long bla = dev->features + ((unsigned long long)1ULL);
  unsigned long l = tp->features;
  return 1;
}


int main(void)
{
   rtl_init_one();
   return 17;
}
