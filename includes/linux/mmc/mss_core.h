/*
 * mss_core.h - MMC/SD/SDIO Core driver header file
 *
 * Copyright (C) 2007 Intel Corporation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License Version 2 only 
 * for now as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * derived from previous mmc code in Linux kernel 
 * Copyright (c) 2002 Hewlett-Packard Company
 * Copyright (c) 2002 Andrew Christian 
 * Copyright (c) 2006 Bridge Wu
 */


#ifndef __MSS_CORE_H__
#define __MSS_CORE_H__

#include <linux/list.h>
#include <linux/device.h>

/* MSS Core general results */
enum mss_result {
	MSS_ERROR_NONE = 0,
	MSS_ERROR_NO_PROTOCOL = 1,
	MSS_ERROR_REGISTER_CARD,
	MSS_ERROR_MISMATCH_CARD,
	MSS_ERROR_WRONG_CARD_TYPE,
	MSS_ERROR_CARD_REMOVING,
	MSS_ERROR_RESP_UNPACK,
	MSS_ERROR_CRC,
	MSS_ERROR_FLASH,
	MSS_ERROR_TIMEOUT,
	MSS_ERROR_LOCKED,
	MSS_ERROR_WP,
	MSS_ERROR_ACTION_UNSUPPORTED,
	MSS_ERROR_DMA,
	MSS_ERROR_WRONG_ARG,
};

#define MSS_MAX_RESPONSE_SIZE	18
//#define MSS_SECTOR_SIZE		512

/**
 *  enum mss_rsp_type: MMC/SD/SDIO command response type.
 *
 *  This is used by both protocol and controller to set MMC/SD/SDIO controller
 *  according to command response type. This is related to MMC/SD/SDIO 
 *  protocol, also to MMC/SD/SDIO controller. So it is placed in core layer,
 *  rather than in protocol layer or controller layer.
 */
enum mss_rsp_type {
	MSS_RESPONSE_NONE = 0,
	MSS_RESPONSE_R1 = 1,
	MSS_RESPONSE_R1B = 10,
	MSS_RESPONSE_R2_CID = 2,
	MSS_RESPONSE_R2_CSD = 20,
	MSS_RESPONSE_R3 = 3,
	MSS_RESPONSE_R4 = 4,
	MSS_RESPONSE_R5 = 5,
	MSS_RESPONSE_R6 = 6,
	MSS_RESPONSE_R7	= 7,
};

enum mss_card_type {
	MSS_UNKNOWN_CARD = 0,
	MSS_MMC_CARD = 1,
	MSS_CEATA,
	MSS_SD_CARD ,
	MSS_SDIO_CARD ,
	MSS_COMBO_CARD ,
	MSS_UNCOMPATIBLE_CARD,
};

struct mss_request {
	struct mss_card		*card;		/* Card */
	int			action;		/* what kind of request */
/* MSS general action in prot_entry() */
#define MSS_ACTION_BASE		0x80
#define MSS_ACTION_PRIVATE	0x100

#define MSS_RECOGNIZE_CARD	(MSS_ACTION_BASE + 1)
#define MSS_INIT_CARD 		(MSS_ACTION_BASE + 2)
#define MSS_READ_MEM		(MSS_ACTION_BASE + 3)
#define MSS_WRITE_MEM		(MSS_ACTION_BASE + 4)
#define MSS_LOCK_CARD		(MSS_ACTION_BASE + 5)
#define MSS_UNLOCK_CARD		(MSS_ACTION_BASE + 6)
#define MSS_QUERY_CARD		(MSS_ACTION_BASE + 7)
#define MSS_GET_CAPACITY	(MSS_ACTION_BASE + 8)

	int			errno;		/* protocl specific error */
	void			*arg;		/* argumnets for request */
	void 			*result;	/* the request results */
};

struct mss_rw_arg {
	u32  	block;		/* Start block address, for sdio, reg addr */
	u32  	nob;		/* Length of block for read/write */
	u32  	block_len;	/* Size of block (sanity check) */
	unsigned int		sg_len;
	struct scatterlist	*sg;
	u8	func;		/* for sdio */
	u8	opcode;		/* for sdio */
	u8	val;		/* for sdio */
};

struct mss_rw_result {
	u32	bytes_xfered;
};

struct mss_cmd {
	u32	opcode;		/* command opcode */
	u32	arg;		/* arguments */
	u8	response[MSS_MAX_RESPONSE_SIZE]; /* Response of the command */
	u32	flags;		/* Specail flags for command */
/* 
 * Some controller needs know some special command.
 * MSS_CMD_INIT: It is the first command after card is reset, and controller
 * may need precede command with some MMC CLKs
 * MSS_CMD_STOP: It is the command that need stop the cuurent data transmission.
 * MSS_CMD_SDIO_EN: The command is sent with the effect that sdio interrupt is 
 * enabled in the controller.
 */
#define MSS_CMD_INIT		(1 << 0)
#define MSS_CMD_STOP		(1 << 1)
#define MSS_CMD_SDIO_EN		(1 << 2)
	
	u32	rtype;		/* response type */
	u32	error;
	struct mss_ll_request *llreq;
	struct mss_data *data;
};

#define MSS_INIT_CMD(cmd, c_opcode, c_arg, c_flags, c_rtype)			\
	do {								\
		(cmd)->opcode	= c_opcode;				\
		(cmd)->arg	= c_arg;				\
		(cmd)->flags	= c_flags;				\
		(cmd)->rtype	= c_rtype;				\
	} while (0)

struct mss_data	{
	u32	timeout_ns;	/* data time out in ns */
	u32	blocks;		/* number of block */
	u32	blksz;		/* block lenght */
	u32	flags;
#define MSS_DATA_WRITE		(1 << 0)
#define MSS_DATA_READ		(1 << 1)
#define MSS_DATA_STREAM		(1 << 2)
#define MSS_DATA_MULTI		(1 << 3)
	
	unsigned int		sg_len;
	struct scatterlist	*sg;
	unsigned int		bytes_xfered;
};

#define MSS_INIT_DATA(data, d_nob, d_block_len, d_flags, d_sg_len, d_sg, \
		d_timeout_ns) 						\
	do {								\
		(data)->timeout_ns	= d_timeout_ns;			\
		(data)->blocks		= d_nob;			\
		(data)->blksz		= d_block_len;			\
		(data)->flags		= d_flags;			\
		(data)->sg_len		= d_sg_len;			\
		(data)->sg		= d_sg;				\
	} while(0)

struct mss_ll_request {
	struct mss_cmd		*cmd;
	struct mss_data		*data;
	
	void			*done_data; 
	void			(*done)(struct mss_ll_request *);
};

struct mss_card {
	u32	card_type;	/* MMC or SD or SDIO */
	u32	state;		/* card states */
/* 
 * Card can be in several states, such as when card is suspended, the states
 * may be MSS_CARD_IDENTIFIED | MSS_CARD_SUSPEND.
 * The states is from the top view, it is diffrent with the states defined in 
 * the protocol.
 */
#define MSS_CARD_REGISTERED	(0x1 << 1)
#define MSS_CARD_REMOVING	(0x1 << 2)
#define MSS_CARD_HANDLEIO	(0x1 << 3)
#define MSS_CARD_SUSPENDED	(0x1 << 4)
#define MSS_CARD_WP		(0x1 << 5)
#define MSS_CARD_INVALID	(0x1 << 6)

	void	*prot_card;	/* point to specific protocl card structure */
	struct mss_prot_driver	*prot_driver;	/* protocol driver */

	u32	usage;	
	u32	bus_width;	 /* 1-bit or 4-bit or 8-bit bus width */

	struct mss_slot	*slot;	 /* in which slot ? */
	struct device	dev;	/* device on mmc_bus */
};

struct mss_driver {	
	struct device_driver	driver;		/* driver on mmc_bus */
	void (*sdio_int_handler)(struct mss_card *);
	void (*request_done)(struct mss_request *);
};

struct mss_slot {
	/* point to real card if inserted, else NULL */
	struct mss_card		*card;
	struct mss_host		*host;		/* point to controller */
	u32			id;		/* slot is, start from 0 */

	struct delayed_work	card_detect;
	void			*private;
};

struct mss_ios {
	u32			clock;		/* current controller clock */ 
	u32			bus_width:4,	/* current bus width */
#define MSS_BUSWIDTH_1BIT	0
#define MSS_BUSWIDTH_4BIT	1
#define MSS_BUSWIDTH_8BIT	2
				access_mode:2,
#define MSS_ACCESS_MODE_BYTE	0
#define MSS_ACCESS_MODE_SECTOR	2

				high_speed:2,
#define MSS_HIGH_SPEED		1
				
				bus_mode:2,	/* current bus mode */
#define MSS_BUSMODE_OPENDRAIN	1
#define MSS_BUSMODE_PUSHPULL	2
	
				chip_select:2,
#define MSS_CS_NO_CARE		0
#define MSS_CS_HIGH		1
#define MSS_CS_LOW		2
	
				power_mode:2,
#define MSS_POWER_OFF		1
#define MSS_POWER_UP		2
#define MSS_POWER_ON		3
				sdio_int:2;
#define MSS_SDIO_INT_DIS	0
#define MSS_SDIO_INT_EN		1

	u32			vdd;
};

/* Host operations */
struct mss_host_ops {
	void	(*request)(struct mss_host *, struct mss_ll_request *);	
	void	(*set_ios)(struct mss_host *, struct mss_ios *);
	/* optioanl operations */
	int	(*is_slot_empty)(struct mss_slot *);	/* slot empty? */
	int	(*is_slot_wp)(struct mss_slot *);	/* write-protected ? */
};

struct mss_host {
	struct list_head	node;		/* list all controllers */
	struct mss_card		*active_card;	/* pointer to active slot */
	unsigned int		slot_num;	/* slots number of controller */
	unsigned int		id;		/* controller id */
	wait_queue_head_t	wq;		/* wait queue for host claim */
	unsigned int		max_seg_size;
	unsigned int		max_hw_segs;
	unsigned int		max_phys_segs;
	unsigned int		max_sectors;
	spinlock_t		lock;
	void			*private;
	struct device		*dev;

	u32			f_min;	/* min clock supported */
	u32			f_max;	/* max clock supported */
	u32			vdd: 24,	/* support VDD */
#define MSS_VDD_170_195	(1 << 7)	/* VDD voltage 1.45 - 1.50 */
#define MSS_VDD_20_21	(1 << 8)	/* VDD voltage 2.0 ~ 2.1 */
#define MSS_VDD_21_22	(1 << 9)	/* VDD voltage 2.1 ~ 2.2 */
#define MSS_VDD_22_23	(1 << 10)	/* VDD voltage 2.2 ~ 2.3 */
#define MSS_VDD_23_24	(1 << 11)	/* VDD voltage 2.3 ~ 2.4 */
#define MSS_VDD_24_25	(1 << 12)	/* VDD voltage 2.4 ~ 2.5 */
#define MSS_VDD_25_26	(1 << 13)	/* VDD voltage 2.5 ~ 2.6 */
#define MSS_VDD_26_27	(1 << 14)	/* VDD voltage 2.6 ~ 2.7 */
#define MSS_VDD_27_28	(1 << 15)	/* VDD voltage 2.7 ~ 2.8 */
#define MSS_VDD_28_29	(1 << 16)	/* VDD voltage 2.8 ~ 2.9 */
#define MSS_VDD_29_30	(1 << 17)	/* VDD voltage 2.9 ~ 3.0 */
#define MSS_VDD_30_31	(1 << 18)	/* VDD voltage 3.0 ~ 3.1 */
#define MSS_VDD_31_32	(1 << 19)	/* VDD voltage 3.1 ~ 3.2 */
#define MSS_VDD_32_33	(1 << 20)	/* VDD voltage 3.2 ~ 3.3 */
#define MSS_VDD_33_34	(1 << 21)	/* VDD voltage 3.3 ~ 3.4 */
#define MSS_VDD_34_35	(1 << 22)	/* VDD voltage 3.4 ~ 3.5 */
#define MSS_VDD_35_36	(1 << 23)	/* VDD voltage 3.5 ~ 3.6 */

#define MSS_VDD_20_26	(0x3f << 8)	/* VDD voltage 2.0 to 2.6 */
#define MSS_VDD_27_36	(0x1ff << 15)	/* VDD voltage 2.7 to 3.6 */
#define MSS_VDD_MASK	(0xffff1 << 7)

				bus_width:4; /* Support max bus width */
	u32			sd_spec:8,
				mmc_spec:8,
				sdio_spec:8,
				high_capacity:1,
				high_speed:1;
#define MSS_SD_SPEC_NONE	0
#define MSS_SD_SPEC_10		1
#define MSS_SD_SPEC_11		2
#define MSS_SD_SPEC_20		3
#define MSS_MMC_SPEC_NONE	0	/* Do not support */
#define MSS_MMC_SPEC_11_12	1	/* MMC specification 1.1 - 1.2 */
#define MSS_MMC_SPEC_14		2	/* MMC specification 1.4 */
#define MSS_MMC_SPEC_20_22	3	/* MMC specification 2.0 - 2.2 */
#define MSS_MMC_SPEC_31		4	/* MMC specification 3.1 */
#define MSS_MMC_SPEC_40_42	5	/* MMC specification 4.0 - 4.2 */
#define MSS_SDIO_SPEC_NONE	0
#define MSS_SDIO_SPEC_10	1
#define MSS_SDIO_SPEC_11	2

	struct mss_ios		ios;
	struct mss_host_ops	*ops;
	struct mss_slot 	slots[0];
};

struct mss_prot_driver {
	char			*name;			/* protocol name */
	struct list_head 	node; 
	
	int	(*prot_entry)(struct mss_card *, unsigned int, void *, void *);
	int	(*attach_card) (struct mss_card *);
	void 	(*detach_card) (struct mss_card *);
	int	(*get_errno) (struct mss_card *);
};

static inline u32 unstuff_bits(u8 *resp, u32 start, u32 size, u32 bytes)
{		
	u32 _end = bytes - (start / 8) - 1;
	u32 _start = bytes - ((start + size - 1) / 8) - 1;
	u8 shft = start % 8;
      	u32 mask = (1 << (((start + size) % 8) ? ((start + size) % 8) : 8)) - 1;
	u32 res = 0;
	
	while (1) {
		res = res << 8;
		res |= resp[_start];
		if (_end != _start)
			_start++;
		else
			break;
		mask = mask << 8 | 0xFF;
	}
	res = (res & mask) >> shft;

	return res;
}

/* to controller driver */
void mss_detect_change(struct mss_host *, unsigned long delay, unsigned int);
int mss_scan_slot(struct work_struct *work);
void mss_scan_host(struct mss_host *);
struct mss_host * mss_alloc_host(unsigned int , unsigned int , unsigned int );
void mss_free_host(struct mss_host *);
struct mss_host *mss_find_host(int );
void mss_force_card_remove(struct mss_card *);
int register_mss_host(struct mss_host *);
void unregister_mss_host(struct mss_host *);

/* to card driver */
int register_mss_driver (struct mss_driver *);
void unregister_mss_driver (struct mss_driver *);
int mss_send_request(struct mss_request *);
unsigned int mss_get_capacity(struct mss_card *);
int mss_card_get(struct mss_card *);
void mss_card_put(struct mss_card *);

/* to protocol driver */
int register_mss_prot_driver(struct mss_prot_driver *);
void unregister_mss_prot_driver(struct mss_prot_driver *);
int mss_send_ll_req(struct mss_host *, struct mss_ll_request *);
int mss_send_simple_ll_req(struct mss_host *, struct mss_ll_request *, struct mss_cmd *, u32 , u32 , u32 , u32 );
void mss_set_sdio_int(struct mss_host *, int );
void mss_set_buswidth(struct mss_host *, int );
void mss_set_busmode(struct mss_host *, int );
void mss_set_clock(struct mss_host *, int );

#ifdef CONFIG_MMC_DEBUG
/* debug for mss_core */
#define dbg(slot, format, arg...) 					\
		do {							\
			printk(KERN_INFO "%s(): host%u slot%u, " 	\
				format "\n", __func__,			\
				(slot)->host->id, (slot)->id, ##arg);	\
		} while(0)

/*
#define dbg(format, arg...) 						\
		do {							\
			printk(KERN_INFO "%s(): " format "\n", 		\
				__func__, ##arg);			\
		} while(0)

*/
/* debug for protocol */
#define dbg2(slot, prot, format, arg...) printk(KERN_INFO "%s(): " \
		"host%u slot%u prot%s, " format "\n", __func__, \
		slot?slot->host->id:-1, slot?slot->id:-1, \
		prot?prot->name:"NULL", ##arg)

/* debug for hw */
#define dbg4(slot, format, arg...) do { \
		printk(KERN_INFO "%s(): ", __func__); \
		if (slot) \
			printk(KERN_INFO "host%u slot%u , ", slot->host->id, \
					slot->id); \
		printk(KERN_INFO format "\n", ##arg); \
	} while(0)

#define dbg5(format, arg...) printk(KERN_INFO "%s(): " format "\n", __func__, ##arg)

#else
#define dbg(slot, format, arg...) 
#define dbg2(slot, prot, format, arg...) 
#define dbg4(slot, format, arg...) 
#define dbg5(format, arg...)
#endif
#endif
