sed -i.bak 's/uchar spin_c_typ/unsigned int spin_c_typ/' pan.c
sed -i.bak 's/provided(int, uchar/provided(int, unsigned int/' pan.c
sed -i.bak 's/provided(int II, unsigned char/provided(int II, unsigned int/' pan.t
sed -i.bak 's/define MAXPROC	255/define MAXPROC	1000/' pan.h
