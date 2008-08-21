/*
 * sdio_protocol.h - SDIO Protocol driver header file
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


#ifndef __SDIO_PROTOCOL_H__
#define __SDIO_PROTOCOL_H__

#define SDIO_PROTOCOL "sdio"

#define SDIO_READ 0
#define SDIO_WRITE 1

#define CMD53_OP_FIXED 0
#define CMD53_OP_INC 1

/* Standard SD clock speeds */
#define SDIO_CARD_CLOCK_SLOW     0     /* 0/100KHz -- 400 kHz for initial setup */
#define SDIO_CARD_CLOCK_FAST  25000000      /* 25 MHz for maximum for normal operation */

/* command type: ac(addressed command), bcr(broadcase command with response),
 *               adtc(addressed data transfer command), bc(broadcase command)
 */

/* Standard SD commands (1.01)           type  argument     response */
   /* class 0, basic: 0, 2-4, 7, 9, 10, 12, 13, 15 */
#define	SD_GO_IDLE_STATE         0   /* bc                          */
#define SD_ALL_SEND_CID          2   /* bcr                     R2  */
#define SD_SET_RELATIVE_ADDR     3   /* ac   [31:16] RCA        R1  */
#define SD_SET_DSR               4   /* bc   [31:16] RCA            */
#define SD_SELECT_CARD           7   /* ac   [31:16] RCA        R1  */
#define SD_SEND_CSD              9   /* ac   [31:16] RCA        R2  */
#define SD_SEND_CID             10   /* ac   [31:16] RCA        R2  */
#define SD_STOP_TRANSMISSION    12   /* ac                      R1b */
#define SD_SEND_STATUS	        13   /* ac   [31:16] RCA        R1  */
#define SD_GO_INACTIVE_STATE    15   /* ac   [31:16] RCA            */

  /* class 1: reserved */
  /* class 3: reserved */

  /* class 2, block read: 16-18 */
#define SD_SET_BLOCKLEN         16   /* ac   [31:0] block len   R1  */
#define SD_READ_SINGLE_BLOCK    17   /* adtc [31:0] data addr   R1  */
#define SD_READ_MULTIPLE_BLOCK  18   /* adtc [31:0] data addr   R1  */

  /* class 4, block write: 16, 24, 25, 27 */
#define SD_WRITE_BLOCK          24   /* adtc [31:0] data addr   R1  */
#define SD_WRITE_MULTIPLE_BLOCK 25   /* adtc [31:0] data addr   R1  */
#define SD_PROGRAM_CID          26   /* adtc                    R1  */
#define SD_PROGRAM_CSD          27   /* adtc                    R1  */
/* CMD26 appeared in SDMC state diagram(data transfer mode) */

  /* class 6, write-protection: 28, 29, 30 */
#define SD_SET_WRITE_PROT       28   /* ac   [31:0] data addr   R1b */
#define SD_CLR_WRITE_PROT       29   /* ac   [31:0] data addr   R1b */
#define SD_SEND_WRITE_PROT      30   /* adtc [31:0] wpdata addr R1  */

  /* class 5, erase: 32, 33, 38 */
#define SD_ERASE_WR_BLOCK_START 32   /* ac   [31:0] data addr   R1  */
#define SD_ERASE_WR_BLOCK_END   33   /* ac   [31:0] data addr   R1  */
#define SD_ERASE                38   /* ac                      R1b */

  /* class 7, lock card: 42 */
#define SD_LOCK_UNLOCK          42   /* adtc                    R1b */

  /* class 8, application specific: 55, 56 */
#define SD_APP_CMD              55   /* ac   [31:16] RCA        R1  */
#define SD_GEN_CMD              56   /* adtc [0] RD/WR          R1b */

  /* class 8, application specific: ACMD6,13,22,23,41,42,51 */
#define SD_SET_BUS_WIDTH		 6   /* ac   [1:0] BUS WIDTH	R1  */
#define SD_SD_STATUS			13   /* adtc                    R1  */
#define SD_SEND_NUM_WR_BLOCKS		22   /* adtc                    R1  */
#define SD_SET_WR_BLK_ERASE_COUNT 	23   /* ac   [22:0] No.Blocks   R1  */
#define SD_SD_SEND_OP_COND		41   /* bcr  [31:0] OCR         R3  */
#define SD_SET_CLR_CARD_DETECT  	42   /* ac   [0] set_cd         R1  */
#define SD_SEND_SCR			51   /* adtc 			R1  */



/* SDIO cmd */
#define IO_SEND_OP_COND		 5    /* bcr  [23:0] OCR	 R4  */
#define IO_RW_DIRECT		 52   /*                         R5  */
#define IO_RW_EXTENDED		 53   /*			 R5  */

/*
  SD card status in R1: compatible to the MMC protocol
  (SD status: extended status field of 512b, ACMD13 require card to send this)
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
#define R1_ERASE_SEQ_ERROR  	(1 << 28)	/* er, c */
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
#define R1_CARD_ECC_DISABLED	(1 << 14)	/* sx, a */
#define R1_ERASE_RESET		(1 << 13)	/* sr, c */
#define R1_STATUS(x)        	(x & 0xFFFFE000)

#define R1_CURRENT_STATE(x) ((x & 0x00001E00) >> 9)	/* sx, b (4 bits) */
#define R1_READY_FOR_DATA	(1 << 8)	/* sx, a */
#define R1_APP_CMD		(1 << 5)	/* sr, c */
#define R1_AKE_SEQ_ERROR	(1 << 3)	/* er, c */


/**
 * SD status: extended status field of 512b, ACMD13 require card to send this.
 * [511:510]	DAT_BUS_WIDTH		SR	A
 * [509]	SECURE_MODE		SR	A
 * [495:480]	SD_CARD_TYPE		SR	A
 * [479:448]	SIZE_OF_PROTECTED_AREA	SR	A
 */

/* for SDIO response r5 r6*/
#define R5_COM_CRC_ERROR	(1 << 7)	/* er, b */
#define R5_ILLEGAL_COMMAND	(1 << 6)	/* er, b */
#define R5_ERROR		(1 << 3) 	/* er, c */
#define R5_FUNCTION_NUMBER	(1 << 1)	/* er, c */
#define R5_OUT_OF_RANGE		(1 << 0)	/* er, c */
#define R5_CURRENT_STATE(x)    	((x & 0x30) >> 4)	/* sx, b (4 bits) */

#define R6_COM_CRC_ERROR	(1 << 15)	/* er, b */
#define R6_ILLEGAL_COMMAND	(1 << 14)	/* er, b */
#define R6_ERROR		(1 << 13)	/* erx, c */
#define R6_STATUS(x)            (x & 0xFFFFE000)
#define R6_CURRENT_STATE(x)    	((x & 0x00001E00) >> 9)	/* sx, b (4 bits) */
#define R6_READY_FOR_DATA	(1 << 8)	/* sx, a */
#define R6_APP_CMD		(1 << 5)	/* sr, c */


#define MAX_FUNC_NUMBER 8

/* cccr register */
#define	CCCR_SDIO_REVISION	(0x00)
#define	SD_SPEC_REVISION	(0x01)
#define	IO_ENABLE		(0x02)
#define	IO_READY		(0x03)
#define	INT_ENABLE		(0x04)
#define	INT_PENDING		(0x05)
#define	IO_ABORT		(0x06)
#define	BUS_IF_CTRL		(0x07)
#define	CARD_CAPABILITY		(0x08)
#define	COMMON_CIS_POINTER_1	(0x09)
#define	COMMON_CIS_POINTER_2	(0x0a)
#define	COMMON_CIS_POINTER_3	(0x0b)
#define	BUS_SUSPEND		(0x0c)
#define	FUNCTION_SELECT		(0x0d)
#define	EXEC_FLAGS		(0x0e)
#define	READY_FLAGS		(0x0f)
#define	FN0_BLKSZ_1		(0x10)
#define	FN0_BLKSZ_2		(0x11)
#define	POWER_CTRL		(0x12)

/* fbr register */
#define FNn_IF_CODE(n)		((n << 8) | 0x00)
#define FNn_EXT_IF_CODE(n)	((n << 8) | 0x01)
#define FNn_SPS(n)		((n << 8) | 0x02)
#define FNn_CIS_POINTER_1(n)	((n << 8) | 0x09)
#define FNn_CIS_POINTER_2(n)	((n << 8) | 0x0a)
#define FNn_CIS_POINTER_3(n)	((n << 8) | 0x0b)
#define FNn_CSA_POINTER_1(n)	((n << 8) | 0x0c)
#define FNn_CSA_POINTER_2(n)	((n << 8) | 0x0d)
#define FNn_CSA_POINTER_3(n)	((n << 8) | 0x0e)
#define FNn_CSA_ACCESS_WIN(n)	((n << 8) | 0x0f)
#define FNn_BLKSZ_1(n)		((n << 8) | 0x10)
#define FNn_BLKSZ_2(n)		((n << 8) | 0x11)

/* CIS tuple codes supported by SDIO device */
#define CISTPL_NULL		(0x00)
#define CISTPL_CHECKSUM		(0x10)
#define CISTPL_VERS_1		(0x15)
#define CISTPL_ALTSTR		(0x16)
#define CISTPL_MANFID		(0x20)
#define CISTPL_FUNCID		(0x21)
#define CISTPL_FUNCE		(0x22)
#define CISTPL_SDIO_STD		(0x91)
#define CISTPL_SDIO_EXT		(0x92)
#define CISTPL_END		(0xff)

#define SDIO_R	(0)
#define SDIO_W	(1)

enum sdio_status_error {
	SDIO_ERROR_NONE = 0,
	SDIO_ERROR_COM_CRC,
	SDIO_ERROR_ILLEGAL_COMMAND, /* 10 */
	SDIO_ERROR_GENERAL,
	SDIO_ERROR_OUT_OF_RANGE,
	SDIO_ERROR_FUNC_NUM,
	SDIO_ERROR_STATE_MISMATCH,
	SDIO_ERROR_HEADER_MISMATCH,
};



enum card_state {
	CARD_STATE_EMPTY = -1,
	CARD_STATE_INIT	 = 0,
	CARD_STATE_STBY	 = 3,
	CARD_STATE_TRAN	 = 4,
	CARD_STATE_CMD	 = 9
};


struct sdio_response_r1 {
	u8  cmd;
	u32 status;
};

struct sdio_response_r4 {
	u32 ocr;
	u8 ready;
	u8 mem_present;
    	u8 func_num;
};

struct sdio_response_r5 {
	u8  cmd;
	u8  status;
	u8  data;
};

struct sdio_response_r6 {
	u8  cmd;
	u16 rca;
	int status;
};

struct sdio_cccr {
	u8	cccrx:4,
		sdiox:4;
	u8	sdx;
	u8	ioex;
	u8	iorx;
	u8	intex;
	u8	intx;
	u8	buswidth:2,
		cd:1;
	/* card capability */
	u8	sdc:1,
		smb:1,
		srw:1,
	    	sbs:1,
		s4mi:1,
	 	e4mi:1,
		lsc:1,
       		ls4b:1;		    
	/* pointer to common CIS */
	u32	pccis;
	u8	br:1,
		bs:1;
	u8	fsx:4,
		df:1;
	u8	exx;
	u8	rfx;
	u16	fn0_blksz;
	u8	smpc:1,
		empc:1;
};

struct sdio_fbr {
	u8	fun_if_code:4,
		fun_support_csa:1,
		fun_csa_enable:1;
	u8	fun_ext_if_code;
	u32	pfcis;
	u32	pcsa;
	u16	fn_blksz;
};

struct sdio_ccis {
	u16	manufacturer; /* CISTPL_MANFID */
	u16	card_id;
#if 1
	/* it seems this is n/a for type common according to SDIO spec 1.1 */
	u8	device_class; /* CISTPL_FUNCID, should be 0x0c */

	/* CISTPL_FUNCE tuple for function 0 */
	u16	fn0_block_size; 
	u8	max_tran_speed;
#endif
};

struct sdio_fcis { 
	u8	device_class; /* CISTPL_FUNCID, should be 0x0c */

	/* CISTPL_FUNCE for function 1 - 7 */
	u8	func_info;
	u8	std_io_rev;
	u32	card_psn;
	u32	csa_size;
	u8	csa_property;
	u16	max_blk_size;
	u32	ocr;
	u8	op_min_pwr; /* op:operating pwr: current in mA */
	u8	op_avg_pwr;
	u8	op_max_pwr;
	u8	sb_min_pwr; /* sb:standby */
	u8	sb_avg_pwr;
	u8	sb_max_pwr;
	u16	min_bw; /* min data transfer bandwidth */
	u16	opt_bw; /* optimum data transfer bandwidth */
	/* SDIO 1.0 not support and 1.1 suport */
	u16	enable_timeout_val;
	/* added for 1.1 on 7.13 */
	u16  sp_avg_pwr;
	u16  sp_max_pwr;
	u16  hp_avg_pwr;
	u16  hp_max_pwr;
	u16  lp_avg_pwr;
	u16  lp_max_pwr;
};

struct sdio_card {
	u8 func_num;
	u8 mem_present;
	u16 rca;
	struct sdio_cccr	cccr;
	struct sdio_fbr	fbr[MAX_FUNC_NUMBER];
	struct sdio_ccis	ccis;
	struct sdio_fcis	fcis[MAX_FUNC_NUMBER];
	struct mss_ll_request	llreq;
	struct mss_cmd		cmd;
	struct mss_data		data;
	enum sdio_status_error	errno;
	enum card_state		state; 
};

#endif
