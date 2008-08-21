/*
 * mmc_protocol.h - MMC Protocol driver header file
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


#ifndef __MMC_PROTOCOL_H__
#define __MMC_PROTOCOL_H__


#include <linux/types.h>

#define MMC_PROTOCOL "mmc"

/*card status*/

#define PARSE_U32(_buf) \
	(((u32)(_buf)[0]) << 24) | (((u32)(_buf)[1]) << 16) | \
        (((u32)(_buf)[2]) << 8) | ((u32)(_buf)[3]);

#define PARSE_U16(_buf) \
	(((u16)(_buf)[0]) << 8) | ((u16)(_buf)[1]);


/* Standard MMC clock speeds */
#define MMC_CARD_CLOCK_SLOW	0		/* The LOWEST clock for mmc */
#define MMC_CARD_CLOCK_FAST	52000000	/* The HIGHEST clock for mmc */

/* command type: ac(addressed command), bcr(broadcase command with response),
 *               adtc(addressed data transfer command), bc(broadcase command)
 */
 
/* Standard MMC commands (4.0)           type  argument     response */

   /* class 0, basic: 0-10, 12-15, 19 */
#define	MMC_GO_IDLE_STATE         0   /* bc                          */
#define MMC_SEND_OP_COND          1   /* bcr  [31:0] OCR         R3  */
#define MMC_ALL_SEND_CID          2   /* bcr                     R2  */
#define MMC_SET_RELATIVE_ADDR     3   /* ac   [31:16] RCA        R1  */
#define MMC_SET_DSR               4   /* bc   [31:16] DSR            */
#define MMC_RSVD5                 5
#define MMC_SWITCH                6   /* ac   see comments below R1b */
#define MMC_SELECT_CARD           7   /* ac   [31:16] RCA        R1  */
#define MMC_SEND_EXT_CSD          8   /* adtc                    R1  */
#define MMC_SEND_CSD              9   /* ac   [31:16] RCA        R2  */
#define MMC_SEND_CID             10   /* ac   [31:16] RCA        R2  */
#define MMC_STOP_TRANSMISSION    12   /* ac                      R1b */
#define MMC_SEND_STATUS	         13   /* ac   [31:16] RCA        R1  */
#define MMC_BUSTEST_R            14   /* adtc                    R1  */
#define MMC_GO_INACTIVE_STATE    15   /* ac   [31:16] RCA            */
#define MMC_BUSTEST_W            19   /* adtc                    R1  */
/* CMD6: [25:24access,23:16index,15:8value,2:0cmd set] */

  /* class 1, stream read: 11 */
#define MMC_READ_DAT_UNTIL_STOP  11   /* adtc [31:0] dadr        R1  */

  /* class 2, block read: 16-18, 23 */
#define MMC_SET_BLOCKLEN         16   /* ac   [31:0] block len   R1  */
#define MMC_READ_SINGLE_BLOCK    17   /* adtc [31:0] data addr   R1  */
#define MMC_READ_MULTIPLE_BLOCK  18   /* adtc [31:0] data addr   R1  */

  /* class 3, stream write: 20 */
#define MMC_WRITE_DAT_UNTIL_STOP 20   /* adtc [31:0] data addr   R1  */

  /* class 4, block write: 16, 23-27 */
#define MMC_SET_BLOCK_COUNT      23   /* ad   [15:0] nob         R1  */
#define MMC_WRITE_BLOCK          24   /* adtc [31:0] data addr   R1  */
#define MMC_WRITE_MULTIPLE_BLOCK 25   /* adtc                    R1  */
#define MMC_PROGRAM_CID          26   /* adtc                    R1  */
#define MMC_PROGRAM_CSD          27   /* adtc                    R1  */

  /* class 6, write protection: 28-30 */
#define MMC_SET_WRITE_PROT       28   /* ac   [31:0] data addr   R1b */
#define MMC_CLR_WRITE_PROT       29   /* ac   [31:0] data addr   R1b */
#define MMC_SEND_WRITE_PROT      30   /* adtc [31:0] wpdata addr R1  */

  /* class 5, erase: 35, 36, 38 */
#define MMC_ERASE_GROUP_START    35   /* ac   [31:0] data addr   R1  */
#define MMC_ERASE_GROUP_END      36   /* ac   [31:0] data addr   R1  */
#define MMC_ERASE                37   /* ac                      R1b */

  /* class 9, I/O mode: 39, 40 */
#define MMC_FAST_IO              39   /* ac   see comments below R4  */
#define MMC_GO_IRQ_STATE         40   /* bcr                     R5  */
/* [31:16RCA, 15:15reg write flag, 14:8reg address, 7:0reg data] */

  /* class 7, lock card: 42 */
#define MMC_LOCK_UNLOCK          42   /* adtc                    R1b */
  
  /* class 8, application specific: 55, 56 */
#define MMC_APP_CMD              55   /* ac   [31:16] RCA        R1  */
#define MMC_GEN_CMD              56   /* adtc [0] RD/WR          R1b */

#if 0
#ifdef CONFIG_CEATA

 // CEATA command 
#define CEATA_RW_MULTIPLE_REGISTER	60	/*		R1B*/
#define CEATA_RW_MULTIPLE_BLOCK		61	/*		R1B*/


//CEATA taskfile related
#define CEATA_FEATURES_EXP		0x01
#define CEATA_SECTOR_COUNT_EXP	0x02
#define CEATA_LBA_LOW_EXP			0x03
#define CEATA_LBA_MID_EXP			0x04
#define CEATA_LBA_HIGH_EXP			0x05
#define CEATA_CONTROL	 			0x06
#define CEATA_FEATURES_ERROR		0x09
#define CEATA_SECTOR_COUNT		0x0A
#define CEATA_LBA_LOW				0x0B
#define CEATA_LBA_MID				0x0C
#define CEATA_LBA_HIGH				0x0D
#define CEATA_DEVICE_HEAD			0x0E
#define CEATA_COMMAND_STATUS		0x0F

//CEATA command arguments to write to taskfile at COMMAND_STATUS(0x0F)
#define CEATA_READ_DMA_EXT			0x25
#define CEATA_WRITE_DMA_EXT			0x35
#define CEATA_STAND_BY_IMMEDIATE		0xE0
#define CEATA_FLUSH_CACHE_EXT			0xEA
#define CEATA_IDENTIFY_DATA			0xEC


//CEATA taskfile file value at the status field(0x0F)
#define BSY					0x80  
#define ATA_ERROR			0x01

#endif
#endif

/*
  MMC status in R1
  Type
  	e : error bit
	s : status bit
	r : detected and set for the actual command response
	x : detected and set during command execution. the host must poll
            the card by sending status command in order to read these bits.
  Clear condition
  	a : according to the card state
	b : always related to the previous command. Reception of
            a valid command will clear it (with a delay of one command)
	c : clear by read
 */

#define R1_OUT_OF_RANGE		(1 << 31)	/* er, c */
#define R1_ADDRESS_ERROR	(1 << 30)	/* erx, c */
#define R1_BLOCK_LEN_ERROR	(1 << 29)	/* er, c */
#define R1_ERASE_SEQ_ERROR      (1 << 28)	/* er, c */
#define R1_ERASE_PARAM		(1 << 27)	/* ex, c */
#define R1_WP_VIOLATION		(1 << 26)	/* erx, c */
#define R1_CARD_IS_LOCKED	(1 << 25)	/* sx, a */
#define R1_LOCK_UNLOCK_FAILED	(1 << 24)	/* erx, c */
#define R1_COM_CRC_ERROR	(1 << 23)	/* er, b */
#define R1_ILLEGAL_COMMAND	(1 << 22)	/* er, b */
#define R1_CARD_ECC_FAILED	(1 << 21)	/* ex, c */
#define R1_CC_ERROR		(1 << 20)	/* erx, c */
#define R1_ERROR		(1 << 19)	/* erx, c */
#define R1_UNDERRUN		(1 << 18)	/* ex, c */
#define R1_OVERRUN		(1 << 17)	/* ex, c */
#define R1_CID_CSD_OVERWRITE	(1 << 16)	/* erx, c, CID/CSD overwrite */
#define R1_WP_ERASE_SKIP	(1 << 15)	/* sx, c */
#define R1_CARD_ECC_DISABLED	(1 << 14)	/* sx, a, must be set 0 to MMC4.0 */
#define R1_ERASE_RESET		(1 << 13)	/* sr, c */

#define R1_STATUS(x)		(x & 0xFFFFE000)

#define R1_CURRENT_STATE(x) ((x & 0x00001E00) >> 9)	/* sx, b (4 bits) */
#define R1_READY_FOR_DATA	(1 << 8)	/* sx, a */
#define R1_SWITCH_ERROR		(1 << 7)	/* E, X */
#define R1_APP_CMD		(1 << 5)	/* sr, c */

#define MMC_SLOT_RCA(slot)	(slot->id + 1)	/* map the slot id to rca */
enum mmc_status_error {
	MMC_ERROR_NONE = 0,
	MMC_ERROR_OUT_OF_RANGE,
	MMC_ERROR_ADDRESS,
	MMC_ERROR_BLOCK_LEN,
	MMC_ERROR_ERASE_SEQ,
	MMC_ERROR_ERASE_PARAM,
	MMC_ERROR_WP_VIOLATION,
	MMC_ERROR_CARD_IS_LOCKED,
	MMC_ERROR_LOCK_UNLOCK_FAILED,
	MMC_ERROR_COM_CRC,
	MMC_ERROR_ILLEGAL_COMMAND, /* 10 */
	MMC_ERROR_CARD_ECC_FAILED,
	MMC_ERROR_CC,
	MMC_ERROR_GENERAL,
	MMC_ERROR_UNDERRUN,
	MMC_ERROR_OVERRUN,
	MMC_ERROR_CID_CSD_OVERWRITE,
	MMC_ERROR_STATE_MISMATCH,
	MMC_ERROR_HEADER_MISMATCH,
};

enum card_state {
	CARD_STATE_EMPTY = -1,
	CARD_STATE_IDLE	 = 0,
	CARD_STATE_READY = 1,
	CARD_STATE_IDENT = 2,
	CARD_STATE_STBY	 = 3,
	CARD_STATE_TRAN	 = 4,
	CARD_STATE_DATA	 = 5,
	CARD_STATE_RCV	 = 6,
	CARD_STATE_PRG	 = 7,
	CARD_STATE_DIS	 = 8,
	CARD_STATE_BTST	 = 9
};

/* These are unpacked versions of the actual responses */

struct mmc_response_r1 {
	u8	cmd;
	u32	status;
};

struct mmc_cid {
	u8	mid;
	u16	oid;
	u8	pnm[7];   /* Product name (we null-terminate) */
	u8	prv;
	u32	psn;
	u8	mdt;
};

struct mmc_csd {
	u8	csd_structure;
#define CSD_STRUCT_1_0	0	/* Valid for system specification 1.0 - 1.2 */
#define CSD_STRUCT_1_1	1	/* Valid for system specification 1.4 - 2.2 */
#define CSD_STRUCT_1_2	2	/* Valid for system specification 3.1 */

	u8	spec_vers;
#define CSD_SPEC_VERS_0	0	/* Implements system specification 1.0 - 1.2 */
#define CSD_SPEC_VERS_1	1	/* Implements system specification 1.4 */
#define CSD_SPEC_VERS_2	2	/* Implements system specification 2.0 - 2.2 */
#define CSD_SPEC_VERS_3	3	/* Implements system specification 3.1 */
#define CSD_SPEC_VERS_4	4	/* Implements system specification 4.0 - 4.2 */

	u8	taac;
	u8	nsac;
	u8	tran_speed;
	u16	ccc;
	u8	read_bl_len;
	u8	read_bl_partial;
	u8	write_blk_misalign;
	u8	read_blk_misalign;
	u8	dsr_imp;
	u16	c_size;
	u8	vdd_r_curr_min;
	u8	vdd_r_curr_max;
	u8	vdd_w_curr_min;
	u8	vdd_w_curr_max;
	u8	c_size_mult;
	union {
		struct { /* MMC system specification version 3.1 */
			u8	erase_grp_size;  
			u8	erase_grp_mult; 
		} v31;
		struct { /* MMC system specification version 2.2 */
			u8	sector_size;
			u8	erase_grp_size;
		} v22;
	} erase;
	u8	wp_grp_size;
	u8	wp_grp_enable;
	u8	default_ecc;
	u8	r2w_factor;
	u8	write_bl_len;
	u8	write_bl_partial;
	u8	file_format_grp;
	u8	copy;
	u8	perm_write_protect;
	u8	tmp_write_protect;
	u8	file_format;
	u8	ecc;
};

struct mmc_ext_csd {
	u8	s_cmd_set;
	u8	sec_count;
	u8	min_perf_w_8_52;
	u8	min_perf_r_8_52;
	u8	min_perf_w_26_4_52;
	u8	min_perf_r_26_4_52;
	u8	min_perf_w_4_26;
	u8	min_perf_r_4_26;
	u8	pwr_cl_26_360;
	u8	pwr_cl_52_360;
	u8	pwr_cl_26_195;
	u8	pwr_cl_52_195;
	u8	card_type;
	u8	csd_structure;
	u8	ext_csd_rev;
	u8	cmd_set;
	u8	cmd_set_rev;
	u8	power_class;
	u8	hs_timing;
	u8	erased_mem_cont;
};

struct mmc_response_r3 {  
	u8	cmd;
	u32	ocr;
#define MMC_ACCESS_MODE_MASK	(0x3 << 29)	/* mask to acess mode */
#define MMC_ACCESS_MODE_SECTOR	(0x2 << 29)	/* sector access mode */
#define MMC_ACCESS_MODE_BYTE	(0x0 << 29)	/* byte access mode */
#define MMC_CARD_BUSY		(1 << 31)	/* Card Power up status bit */
#define MMC_VDD_MASK		(0xffff1 << 7)
}; 

struct mmc_response_r4 { /*Fast I/O */
	u8	cmd;
	u16	rca;
	u8	status;
	u8	reg_addr;
	u8	read_reg_contents;
};

struct mmc_response_r5 { /*Interrupt request */
	u8	cmd;
	u16	rca;
	u16	irq_data;
};

struct mmc_card {
	struct mmc_cid 		cid;
	struct mmc_csd		csd;
	struct mmc_ext_csd	ext_csd;
	/* The mmc card property */
	u32			vdd:24,
				high_speed:2,
				access_mode:2,
				bus_width:4;
	char			*buf;
	struct mss_ll_request	llreq;
	struct mss_cmd		cmd;
	struct mss_data		data;
	enum mmc_status_error	errno;
	enum card_state	state;  /* empty, ident, ready, whatever */
	u32	block_len;
	u16	rca;
};

#endif /* end __MMC_PROTOCOL_H__ */
