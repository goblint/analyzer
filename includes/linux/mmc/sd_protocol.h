/*
 * sd_protocol.h - SD Protocol driver header file
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



#ifndef __SD_PROTOCOL_H__
#define __SD_PROTOCOL_H__

#include <linux/types.h>

#define SD_PROTOCOL "sd"

#define MSS_SD_QUERY_FUNC	0x100
#define MSS_SD_SW_FUNC	0x101

/* Standard SD clock speeds */
#define SD_CARD_CLOCK_SLOW	0	
#define SD_CARD_CLOCK_FAST		50000000	

/* command type: ac(addressed command), bcr(broadcase command with response),
 *               adtc(addressed data transfer command), bc(broadcase command)
 */

/* Standard SD commands (1.01)           type  argument     response */
   /* class 0, basic: 0, 2-4, 7, 9, 10, 12, 13, 15 */
#define	SD_GO_IDLE_STATE         0   /* bc                          */
#define SD_ALL_SEND_CID          2   /* bcr                     R2  */
#define SD_SEND_RELATIVE_ADDR     3   /* ac   [31:16] RCA        R1  */
#define SD_SET_DSR               4   /* bc   [31:16] RCA            */
#define SD_SELECT_CARD           7   /* ac   [31:16] RCA        R1  */
#define SD_SEND_CSD              9   /* ac   [31:16] RCA        R2  */
#define SD_SEND_CID             10   /* ac   [31:16] RCA        R2  */
#define SD_STOP_TRANSMISSION    12   /* ac                      R1b */
#define SD_SEND_STATUS	        13   /* ac   [31:16] RCA        R1  */
#define SD_GO_INACTIVE_STATE    15   /* ac   [31:16] RCA            */
#define SD_SW_FUNC		6
#define SD_SEND_IF_COND		8

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
#define R1_ERASE_SEQ_ERROR	(1 << 28)	/* er, c */
#define R1_ERASE_PARAM		(1 << 27)	/* ex, c */
#define R1_WP_VIOLATION		(1 << 26)	/* erx, c */
#define R1_CARD_IS_LOCKED	(1 << 25)	/* sx, a */
#define R1_LOCK_UNLOCK_FAILED	(1 << 24)	/* erx, c */
#define R1_COM_CRC_ERROR	(1 << 23)	/* er, b */
#define R1_ILLEGAL_COMMAND	(1 << 22)	/* er, b */
#define R1_CARD_ECC_FAILED	(1 << 21)	/* ex, c */
#define R1_CC_ERROR		(1 << 20)	/* erx, c */
#define R1_ERROR		(1 << 19)	/* erx, c */
#define R1_CID_CSD_OVERWRITE	(1 << 16)	/* erx, c, CID/CSD overwrite */
#define R1_WP_ERASE_SKIP	(1 << 15)	/* sx, c */
#define R1_CARD_ECC_DISABLED	(1 << 14)	/* sx, a */
#define R1_ERASE_RESET		(1 << 13)	/* sr, c */
#define R1_STATUS(x)		((x) & 0xFFFFE000)

#define R1_CURRENT_STATE(x)	(((x) & 0x00001E00) >> 9) /* sx, b (4 bits) */
#define R1_READY_FOR_DATA	(1 << 8)	/* sx, a */
#define R1_APP_CMD		(1 << 5)	/* sr, c */
#define R1_AKE_SEQ_ERROR	(1 << 3)	/* er, c */


#define R6_COM_CRC_ERROR	(1 << 15)	/* er, b */
#define R6_ILLEGAL_COMMAND	(1 << 14)	/* er, b */
#define R6_ERROR		(1 << 13)	/* erx, c */
#define R6_STATUS(x)		((x) & 0xFFFFE000)
#define R6_CURRENT_STATE(x)	(((x) & 0x00001E00) >> 9) /* sx, b (4 bits) */
#define R6_READY_FOR_DATA	(1 << 8)	/* sx, a */
#define R6_APP_CMD		(1 << 5)	/* sr, c */

/**
 * SD status: extended status field of 512b, ACMD13 require card to send this.
 * [511:510]	DAT_BUS_WIDTH	SR	A
 * [509]		SECURE_MODE		SR	A
 * [495:480]	SD_CARD_TYPE	SR	A
 * [479:448]	SIZE_OF_PROTECTED_AREA	SR	A
 */
enum sd_status_error {
	SD_ERROR_NONE = 0,
	SD_ERROR_OUT_OF_RANGE,
	SD_ERROR_ADDRESS,
	SD_ERROR_BLOCK_LEN,
	SD_ERROR_ERASE_SEQ,
	SD_ERROR_ERASE_PARAM,
	SD_ERROR_WP_VIOLATION,
	SD_ERROR_CARD_IS_LOCKED,
	SD_ERROR_LOCK_UNLOCK_FAILED,
	SD_ERROR_COM_CRC,
	SD_ERROR_ILLEGAL_COMMAND, /* 10 */
	SD_ERROR_CARD_ECC_FAILED,
	SD_ERROR_CC,
	SD_ERROR_GENERAL,
	SD_ERROR_CID_CSD_OVERWRITE,
	SD_ERROR_AKE_SEQ,
	SD_ERROR_SWFUNC,
	SD_ERROR_STATE_MISMATCH,
	SD_ERROR_HEADER_MISMATCH,
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
	CARD_STATE_CMD	 = 9
};

struct sd_cid {
	u8  mid;
	u16 oid;
	u8  pnm[6];   // Product name (we null-terminate)
	u8  prv;
	u32 psn;
	u16 mdt;
};

struct sd_csd {
	u8  csd_structure;
#define CSD_STRUCT_VER_1_0  0	/* Valid for system specification 1.0 - 1.2 */
#define CSD_STRUCT_VER_1_1  1	/* Valid for system specification 1.4 - 2.2 */
#define CSD_STRUCT_VER_1_2  2	/* Valid for system specification 3.1       */
	u8  taac;
	u8  nsac;
	u8  tran_speed;
	u16 ccc;
	u8  read_bl_len;
	u8  read_bl_partial;
	u8  write_blk_misalign;
	u8  read_blk_misalign;
	u8  dsr_imp;
	union {
		struct {
			u16 c_size;
			u8  vdd_r_curr_min;
			u8  vdd_r_curr_max;
			u8  vdd_w_curr_min;
			u8  vdd_w_curr_max;
			u8  c_size_mult;
		} csd1;
		struct {
			u32 c_size;
		} csd2;
	}csd;
	u8  erase_blk_en;
	u8  sector_size;
	u8  wp_grp_size;
	u8  wp_grp_enable;
	u8  r2w_factor;
	u8  write_bl_len;
	u8  write_bl_partial;
	u8  file_format_grp;
	u8  copy;
	u8  perm_write_protect;
	u8  tmp_write_protect;
	u8  file_format;
};

struct sd_scr {
	u8 scr_structure;
	u8 sd_spec;
	u8 data_stat_after_erase;
	u8 sd_security;
	u8 sd_bus_width;
#define SCR_BUSWIDTH_1BIT	0
#define SCR_BUSWIDTH_4BIT	(1 << 2)
	u8 init;
};

/* These are unpacked versions of the actual responses */

struct sd_response_r1 {
	u8  cmd;
	u32 status;
};

struct sd_response_r3 {  
	u8 cmd;
	u32 ocr;
#define SD_OCR_CARD_BUSY	(1 << 31)	/* Card Power up status bit */
#define SD_OCR_CCS		(1 << 30)
}; 

struct sd_response_r6 {
	u8  cmd;
	u16 rca;
	u16 status;
};

struct sd_response_r7 {
	u8 cmd;
	u8 ver;
	u8 vca;
	u8 pattern;
};

struct sw_func_status {
	u16 current_consumption;
	u16 func_support[6];
	u8 group_status[6];
	u16 func_busy[6];
};

struct sd_card {
	struct sd_cid 	cid;
	struct sd_csd	csd;
	struct sd_scr	scr;
	u32		ocr;
	u32		rca:16,
			ver:8,
			high_speed:2;				   
	char		*buf;
	struct mss_ll_request	llreq;
	struct mss_cmd		cmd;
	struct mss_data		data;
	enum sd_status_error	errno;
	enum card_state		state; 
	u32		block_len;
};

struct io_swfunc_request {
	u16 current_acceptable;
	u8  args[6];	
};

#endif
